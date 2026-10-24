# ============================================================
# KDD-BASED STATISTICAL FRAMEWORK FOR INTERPRETABLE CYBERSECURITY
# INCIDENT ANALYSIS AND DECISION SUPPORT — v7.3.3
# ============================================================

options(shiny.maxRequestSize = 150 * 1024^2)   # 150 MB upload

# ── 0. PAQUETES ──────────────────────────────────────────────
paquetes <- c(
  "shiny","shinydashboard","tidyverse",
  "readr","readxl","lubridate","MASS","performance",
  "broom","DT","ggplot2","scales","FactoMineR",
  "factoextra","psych","tm","wordcloud","RColorBrewer","tidytext",
  "patchwork","cluster","ggrepel","base64enc","writexl","plotly","htmlwidgets"
)
nuevos <- paquetes[!paquetes %in% installed.packages()[,"Package"]]
if (length(nuevos)) install.packages(nuevos, repos="https://cloud.r-project.org")

suppressPackageStartupMessages({
  # MASS primero, luego tidyverse lo sobreescribe (evita masking de select/filter)
  library(MASS)
  library(shiny); library(shinydashboard)
  library(tidyverse)  # dplyr carga DESPUES de MASS -> dplyr gana el namespace
  library(readr); library(readxl)
  library(lubridate); library(performance)
  library(broom); library(DT); library(ggplot2); library(scales)
  library(FactoMineR); library(factoextra); library(psych)
  library(tm); library(wordcloud); library(RColorBrewer); library(tidytext)
  library(patchwork); library(cluster); library(ggrepel)
  library(base64enc); library(writexl); library(plotly); library(htmlwidgets)
})

# Anclar funciones de dplyr/ggplot2 que otros paquetes podrian enmascarar
select    <- dplyr::select
filter    <- dplyr::filter
rename    <- dplyr::rename
mutate    <- dplyr::mutate
group_by  <- dplyr::group_by
summarise <- dplyr::summarise
summarize <- dplyr::summarize
count     <- dplyr::count
aes       <- ggplot2::aes

# ── 0C. IDIOMA DE LA INTERFAZ / INTERFACE LANGUAGE [v7.3.3] ──────
# Toda la interfaz se muestra en un solo idioma, elegido en la barra
# lateral (Español / English). Al cambiarlo, la app se recarga.
# Los textos se traducen al mostrarse (menús, títulos, tablas, gráficos,
# mensajes); el código y los datos internos no cambian.
# Las figuras del artículo (pestaña Publicación, reproducir_articulo() y
# la descarga de la Figura 11) se exportan siempre en inglés.
# Para agregar otro idioma: añadir una columna a I18N y su opción en
# APP_LANGS (por ejemplo pt = "Português").
APP_LANGS        <- c("Español" = "es", "English" = "en")
APP_LANG_DEFAULT <- "es"

.i18n_state <- new.env(parent = emptyenv())
.i18n_state$lang <- APP_LANG_DEFAULT

i18n_lang <- function() .i18n_state$lang

i18n_set_lang <- function(lang) {
  if (is.null(lang) || !length(lang) || !lang %in% APP_LANGS) lang <- APP_LANG_DEFAULT
  .i18n_state$lang <- lang
  .i18n_state$map  <- NULL
  invisible(lang)
}

i18n_lang_from_query <- function(qs) {
  q <- tryCatch(shiny::parseQueryString(if (is.null(qs)) "" else qs), error = function(e) list())
  if (!is.null(q$lang) && q$lang %in% APP_LANGS) q$lang else APP_LANG_DEFAULT
}

# Builds the lookup for the active language: every text in another
# language is mapped to its version in the active language.
.i18n_map <- function() {
  if (!is.null(.i18n_state$map)) return(.i18n_state$map)
  lang <- i18n_lang()
  tgt  <- I18N[[lang]]
  src_cols <- setdiff(names(I18N), lang)
  from <- unlist(lapply(src_cols, function(cc) I18N[[cc]]), use.names = FALSE)
  to   <- rep(tgt, times = length(src_cols))
  keep <- !is.na(from) & !is.na(to) & nzchar(from) & from != to
  from <- from[keep]; to <- to[keep]
  # Also match fragments without their leading/trailing spaces or line breaks
  tf <- trimws(from); tt <- trimws(to)
  add <- tf != from & nchar(tf) >= 4 & tf != tt
  from <- c(from, tf[add]); to <- c(to, tt[add])
  dup  <- duplicated(from)
  from <- from[!dup]; to <- to[!dup]
  # Phrases (several words, punctuation or fragments with spaces) can be
  # replaced inside longer texts; single words only when they are the whole text.
  is_phrase <- grepl("[[:space:]]", from) & nchar(trimws(from)) >= 3
  ord <- order(nchar(from), decreasing = TRUE)
  .i18n_state$map <- list(
    exact_from = from, exact_to = to,
    ph_from = from[ord][is_phrase[ord]], ph_to = to[ord][is_phrase[ord]])
  .i18n_state$map
}

# Translate a character vector (exact match first, then phrases).
tr_text <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  if (is.factor(x)) { levels(x) <- tr_text(levels(x)); return(x) }
  if (!is.character(x)) return(x)
  m <- .i18n_map()
  out <- x
  hit <- match(out, m$exact_from)
  out[!is.na(hit)] <- m$exact_to[hit[!is.na(hit)]]
  todo <- which(is.na(hit) & !is.na(out) & nzchar(out))
  if (length(todo) && length(m$ph_from)) {
    u <- unique(out[todo])                      # translate each distinct text once
    for (k in seq_along(m$ph_from)) {
      w <- grepl(m$ph_from[k], u, fixed = TRUE)
      if (any(w)) u[w] <- gsub(m$ph_from[k], m$ph_to[k], u[w], fixed = TRUE)
    }
    out[todo] <- u[match(out[todo], unique(out[todo]))]
  }
  out
}

# Exact-match only (labels of categories, table cells, column names)
tr_exact <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  if (is.factor(x)) { levels(x) <- tr_exact(levels(x)); return(x) }
  if (!is.character(x)) return(x)
  m <- .i18n_map()
  hit <- match(x, m$exact_from)
  x[!is.na(hit)] <- m$exact_to[hit[!is.na(hit)]]
  x
}

# HTML tags (UI, renderUI, valueBox, notifications)
tr_tag <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "html")) {
    return(htmltools::HTML(tr_text(as.character(x))))
  }
  if (is.character(x)) return(tr_text(x))
  if (inherits(x, "shiny.tag")) {
    if (x$name %in% c("script", "style")) return(x)
    for (a in intersect(names(x$attribs), c("title", "placeholder", "aria-label", "data-original-title")))
      if (is.character(x$attribs[[a]])) x$attribs[[a]] <- tr_text(x$attribs[[a]])
    if (length(x$children)) x$children <- lapply(x$children, tr_tag)
    return(x)
  }
  if (inherits(x, "shiny.tag.list") || (is.list(x) && !is.data.frame(x) && is.null(attr(x, "class")))) {
    cl <- attributes(x)
    y <- lapply(x, tr_tag); attributes(y) <- cl
    return(y)
  }
  if (is.list(x) && inherits(x, "shiny.render.function")) return(x)
  x
}

# Which aesthetics of a ggplot are mapped to discrete data
.tr_discrete_aes <- function(p) {
  out <- character()
  maps <- list(p$mapping); datas <- list(p$data)
  for (l in p$layers) {
    maps  <- c(maps, list(l$mapping))
    datas <- c(datas, list(if (is.data.frame(l$data)) l$data else p$data))
  }
  for (k in seq_along(maps)) {
    m <- maps[[k]]; d <- datas[[k]]
    if (!length(m) || !is.data.frame(d)) next
    for (a in names(m)) {
      a2 <- if (a == "color") "colour" else a
      if (!a2 %in% c("x", "y", "fill", "colour")) next
      v <- tryCatch(rlang::eval_tidy(m[[a]], d), error = function(e) NULL)
      if (!is.null(v) && (is.character(v) || is.factor(v) || is.logical(v))) out <- c(out, a2)
    }
  }
  unique(out)
}

# Translate titles, axis/legend names, text layers and category labels
tr_plot <- function(p) {
  # Compatible with ggplot2 3.x (S3/lists) and ggplot2 >= 4.0 (S7 objects):
  # labels are changed only through `+ labs()`, layers/scales are ggproto
  # environments edited in place, and patchworks use their public API.
  # Any failure returns the original plot untouched (never breaks a figure).
  p0 <- p
  tryCatch({
    if (inherits(p, "patchwork")) {
      n <- tryCatch(length(p), error = function(e) 0L)
      for (i in seq_len(n)) {
        sub <- tryCatch(p[[i]], error = function(e) NULL)
        if (!is.null(sub)) p[[i]] <- tr_plot(sub)
      }
      ann <- tryCatch(p$patches$annotation, error = function(e) NULL)
      if (length(ann)) {
        a2 <- list()
        for (f in c("title", "subtitle", "caption"))
          if (is.character(ann[[f]])) a2[[f]] <- tr_text(ann[[f]])
        if (length(a2)) p <- p + do.call(patchwork::plot_annotation, a2)
      }
      return(p)
    }
    if (!inherits(p, "ggplot")) return(p)
    # 1) titles / axis / legend labels
    labs_now <- tryCatch({
      if (exists("get_labs", envir = asNamespace("ggplot2"), inherits = FALSE))
        as.list(ggplot2::get_labs(p))           # ggplot2 >= 3.5.2 / 4.x: resolved labels
      else as.list(unclass(p$labels))           # older ggplot2
    }, error = function(e) tryCatch(as.list(unclass(p$labels)), error = function(e2) list()))
    labs_new <- list()
    for (nm in names(labs_now)) {
      v <- labs_now[[nm]]
      if (is.character(v) && length(v) == 1L && !is.na(v)) {
        tv <- tr_text(v); if (!identical(tv, v)) labs_new[[nm]] <- tv
      }
    }
    if (length(labs_new)) p <- p + do.call(ggplot2::labs, labs_new)
    # 2) text layers (ggproto environments: edited in place)
    lab_cols <- c("label", "lbl", "lab", "txt", "texto", "etiqueta")
    for (ly in p$layers) {
      lp <- ly$aes_params
      if (!is.null(lp$label) && is.character(lp$label)) { lp$label <- tr_text(lp$label); ly$aes_params <- lp }
      ld <- ly$data
      if (is.data.frame(ld)) {
        cc <- intersect(names(ld), lab_cols)
        if (length(cc)) { for (k in cc) if (is.character(ld[[k]])) ld[[k]] <- tr_text(ld[[k]]); ly$data <- ld }
      }
    }
    # 3) label columns in the plot's own data
    pd <- tryCatch(p$data, error = function(e) NULL)
    if (is.data.frame(pd)) {
      cc <- intersect(names(pd), lab_cols)
      if (length(cc)) {
        for (k in cc) if (is.character(pd[[k]])) pd[[k]] <- tr_text(pd[[k]])
        p <- tryCatch({ p$data <- pd; p }, error = function(e)
               tryCatch(ggplot2::`%+%`(p, pd), error = function(e2) p))
      }
    }
    # 4) explicit scales: names and discrete category labels
    for (s in p$scales$scales) {
      if (is.character(s$name)) s$name <- tr_text(s$name)
      if (inherits(s, "ScaleDiscrete") && inherits(s$labels, "waiver")) s$labels <- tr_exact
    }
    # 5) implicit discrete scales: add one with translated category labels
    for (a in .tr_discrete_aes(p)) {
      if (p$scales$has_scale(a)) next
      p <- p + switch(a,
        x = ggplot2::scale_x_discrete(labels = tr_exact),
        y = ggplot2::scale_y_discrete(labels = tr_exact),
        fill = ggplot2::scale_fill_discrete(labels = tr_exact),
        colour = ggplot2::scale_colour_discrete(labels = tr_exact))
    }
    if (is.function(.i18n_state$hook)) .i18n_state$hook(p)   # used only by automated tests
    p
  }, error = function(e) p0)
}

# Plotly figures: titles, axis titles, legend/trace names, hover text
tr_plotly <- function(w) {
  if (!inherits(w, "plotly")) return(w)
  walk <- function(z) {
    if (is.character(z)) return(tr_text(z))
    if (is.list(z)) { nm <- names(z); z <- lapply(z, walk); names(z) <- nm }
    z
  }
  b <- plotly::plotly_build(w)
  b$x$layout <- walk(b$x$layout)
  b$x$data <- lapply(b$x$data, function(tr) {
    for (f in intersect(names(tr), c("name", "text", "hovertext", "hovertemplate", "legendgroup")))
      tr[[f]] <- walk(tr[[f]])
    tr
  })
  b
}

# Tables: column names, character cells and caption
tr_df <- function(d) {
  if (!is.data.frame(d)) return(d)
  names(d) <- tr_exact(names(d))
  for (cc in seq_along(d)) {
    v <- d[[cc]]
    if (is.factor(v) || is.character(v)) d[[cc]] <- tr_text(v)
  }
  d
}
tr_dt <- function(x) {
  if (is.data.frame(x)) return(tr_df(x))
  if (inherits(x, "datatables")) {
    if (is.data.frame(x$x$data)) {
      d <- x$x$data
      for (cc in seq_along(d)) if (is.factor(d[[cc]]) || is.character(d[[cc]])) d[[cc]] <- tr_text(d[[cc]])
      x$x$data <- d
    }
    if (is.character(x$x$container)) {
      cont <- x$x$container
      hdr <- regmatches(cont, gregexpr(">([^<>]+)<", cont))[[1]]
      for (h in unique(hdr)) {
        inner <- substr(h, 2, nchar(h) - 1)
        tt <- tr_exact(trimws(inner))
        if (!identical(tt, trimws(inner))) cont <- gsub(h, paste0(">", tt, "<"), cont, fixed = TRUE)
      }
      x$x$container <- cont
    }
    if (!is.null(x$x$caption)) {
      cap <- x$x$caption
      if (is.character(cap)) {
        un <- function(z) gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", z, fixed = TRUE), fixed = TRUE), fixed = TRUE)
        es <- function(z) gsub(">", "&gt;", gsub("<", "&lt;", z, fixed = TRUE), fixed = TRUE)
        inner <- sub("^(\\s*<caption[^>]*>)?(.*?)(</caption>\\s*)?$", "\\2", cap, perl = TRUE)
        x$x$caption <- sub(inner, es(tr_text(un(inner))), cap, fixed = TRUE)
      } else x$x$caption <- tr_tag(cap)
    }
  }
  x
}

# Wrap a shiny render function so its value is translated before display
.i18n_wrap_render <- function(render_fun, post) {
  force(render_fun); force(post)
  function(expr, ..., env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr)
    # (function() expr)() keeps return() inside the original expression working
    render_fun(bquote(.(post)((function() .(expr))())), ..., env = env, quoted = TRUE)
  }
}
.tr_plot_value <- function(x) {
  if (inherits(x, c("ggplot", "patchwork"))) return(tr_plot(x))
  invisible(x)
}
renderPlot     <- .i18n_wrap_render(shiny::renderPlot, .tr_plot_value)
renderUI       <- .i18n_wrap_render(shiny::renderUI, tr_tag)
renderValueBox <- .i18n_wrap_render(shinydashboard::renderValueBox, tr_tag)
renderDT       <- .i18n_wrap_render(DT::renderDT, tr_dt)
renderPrint    <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shiny::renderText(bquote(tr_text(paste(utils::capture.output((function() .(expr))()), collapse = "\n"))),
                    env = env, quoted = TRUE)
}
showNotification <- function(ui, ...) shiny::showNotification(tr_tag(ui), ...)
withProgress <- function(expr, min = 0, max = 1, value = min + (max - min) * 0.1,
                         message = NULL, detail = NULL, style = shiny::getShinyOption("progress.style", default = "notification"),
                         session = shiny::getDefaultReactiveDomain(), env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shiny::withProgress(expr, min = min, max = max, value = value,
                      message = tr_text(message), detail = tr_text(detail), style = style,
                      session = session, env = env, quoted = TRUE)
}
need <- function(expr, message = paste(label, "must be provided"), label) {
  shiny::need(expr, tr_text(message))
}

I18N_PAIRS <- c(
 
)
I18N <- rbind(
  data.frame(es = I18N_PAIRS[c(TRUE, FALSE)], en = I18N_PAIRS[c(FALSE, TRUE)],
             orig = NA_character_, stringsAsFactors = FALSE),
  data.frame(es = I18N_MIXED[seq(2, length(I18N_MIXED), 3)],
             en = I18N_MIXED[seq(3, length(I18N_MIXED), 3)],
             orig = I18N_MIXED[seq(1, length(I18N_MIXED), 3)], stringsAsFactors = FALSE))


# ── 0B. PALETA PASTEL GLOBAL ─────────────────────────────────
# Colores base del tema pastel (usados en toda la app)
PAST <- list(
  azul      = "#85C1E9",  # azul pastel
  azul2     = "#AED6F1",
  azul_osc  = "#7FB3D5",
  rojo      = "#F1948A",  # rojo/salmón pastel
  rojo2     = "#F5B7B1",
  verde     = "#82E0AA",
  verde2    = "#A9DFBF",
  verde_osc = "#7DCEA0",
  naranja   = "#F0B27A",
  naranja2  = "#FAD7A0",
  amarillo  = "#F9E79F",
  morado    = "#C39BD3",
  morado2   = "#C5C3F0",
  turquesa  = "#76D7C4",
  gris_txt  = "#4A4A4A",
  texto     = "#1B2631"
)

# Paleta pastel fija por tipo de ataque
pal_ataque <- c(
  "Phishing"="#F5B7B1","DDoS"="#FAD7A0","Malware"="#C39BD3",
  "Ransomware"="#F1948A","SQL Injection"="#AED6F1",
  "Brute Force"="#A9DFBF","Zero-Day Exploit"="#F9E79F",
  "Cross-Site Scripting"="#A3E4D7"
)

# [v7.3.2] Paleta de los terciles de frecuencia ajustada.
# Se conservan las etiquetas Bajo/Medio/Alto por compatibilidad,
# pero designan terciles empíricos, NO niveles de riesgo validados.
pal_riesgo  <- c(Bajo="#A9DFBF", Medio="#FAD7A0", Alto="#F5B7B1")
pal_cluster <- c("1"="#AED6F1","2"="#F5B7B1","3"="#A9DFBF",
                 "4"="#FAD7A0","5"="#C39BD3")

# [v7.3.2] Texto estándar de advertencia metodológica para la
# categoría exploratoria derivada de los valores ajustados.
CAT_FREC_TXT <- paste0(
  "Categoría exploratoria: terciles de la frecuencia ajustada por el GLM. ",
  "No corresponde a umbrales de riesgo validados.")

CAT_FREC_PROY_TXT <- paste0(
  "Categoría exploratoria: terciles de la frecuencia proyectada por el GLM, usando como referencia fija los puntos de corte del año base. ",
  "No corresponde a umbrales de riesgo validados.")

# [v7.3.2] Trazabilidad y reproducibilidad del framework.
TECH_LABELS <- c(
  n_incidentes="Incident frequency",
  impacto_total="Economic impact",
  severidad_media="Mean severity",
  t_respuesta_media="Mean response time",
  duracion_media="Mean attack duration",
  tasa_exito="Attack success rate",
  idx_resiliencia="Resilience index"
)

FRAMEWORK_VERSION <- "7.3.3"
# [v7.3.3] Módulo organizacional (cuestionario) y minería de texto:
# desactivados en esta versión porque el artículo de Frontiers usa solo
# datos de incidentes. Cambiar a TRUE para el estudio del cuestionario.
MODULO_ORGANIZACIONAL <- TRUE
# [v7.3.3] Pestaña "Publicación" (exporta las figuras del artículo en
# outputs/article_figures/). TRUE = visible; FALSE = oculta (versión pública
# del repositorio). reproducir_articulo() funciona igual en ambos casos.
MOSTRAR_PUBLICACION <- FALSE
# [v7.3.3] Versión única para el artículo revisado (Frontiers in AI, 2013039):
# incluye en este mismo archivo todos los análisis de sensibilidad, la
# Figura 11 y la función reproducir_articulo(); no se necesitan scripts aparte.
RANDOM_SEED <- 42L

# [v7.3.2] Número de clusters retenido para el análisis final.
# k = 3 se retuvo como solución final considerando conjuntamente
# el método del codo, el coeficiente Silhouette medio y la
# interpretabilidad de los perfiles resultantes. El máximo empírico
# de Silhouette se reporta de forma transparente y puede no coincidir
# con la solución final retenida.
K_FINAL <- 3L

K_FINAL_RATIONALE <- paste0(
  "k = ", K_FINAL,
  " se retuvo como solución final considerando el método del codo, ",
  "el coeficiente Silhouette medio y la interpretabilidad de los perfiles."
)

# ── 1. HELPERS ───────────────────────────────────────────────
dir_crear <- function(x) if (!dir.exists(x)) dir.create(x, recursive=TRUE)
dir_crear("outputs/graficos"); dir_crear("outputs/tablas")
dir_crear("outputs/article_figures/pdf")
dir_crear("outputs/article_figures/jpg")
dir_crear("outputs/article_figures/html")
dir_crear("outputs/article_figures/pdf"); dir_crear("outputs/article_figures/jpg")
dir_crear("outputs/article_figures/html")

guardar_plot <- function(p, nombre)
  tryCatch(ggsave(paste0("outputs/graficos/",nombre,".png"),
                  p, width=9, height=5.5, dpi=300), error=function(e) NULL)

# Mensaje de error/aviso dibujado con base R (100% robusto)
plot_msg <- function(msg, col="gray40") {
  graphics::plot.new()
  graphics::plot.window(xlim=c(0,1), ylim=c(0,1))
  graphics::text(0.5, 0.5, msg, cex=1.3, col=col, adj=c(0.5,0.5))
}

# Tema común para gráficos de publicación (estética pastel/limpia)
tema_pub <- function()
  ggplot2::theme_bw(base_size=13) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(face="bold", size=14, color="#34495E"),
    plot.subtitle = ggplot2::element_text(color="gray45", size=11),
    plot.caption  = ggplot2::element_text(color="gray55", size=9, hjust=0),
    legend.position="right",
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_rect(color="#D5DBDB"),
    strip.background = ggplot2::element_rect(fill="#EBF5FB", color="#D5DBDB")
  )

# HTML autocontenido con la imagen JPG embebida en base64
html_con_imagen <- function(jpg_path, html_path, titulo) {
  b64 <- base64enc::base64encode(jpg_path)
  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><title>", titulo,
    "</title></head>",
    "<body style='font-family:Segoe UI,Arial,sans-serif;background:#FDFEFE;",
    "text-align:center;padding:24px;'>",
    "<h2 style='color:#5D6D7E;font-weight:600;'>", titulo, "</h2>",
    "<img style='max-width:96%;border:1px solid #E5E8E8;border-radius:10px;",
    "box-shadow:0 2px 10px rgba(0,0,0,0.06);' ",
    "src='data:image/jpeg;base64,", b64, "'/>",
    "<p style='color:#95A5A6;font-size:12px;margin-top:14px;'>",
    "Generado automáticamente — Estudio de efectividad de políticas de ",
    "ciberseguridad empresarial</p></body></html>")
  writeLines(html, html_path, useBytes=TRUE)
}

# Exportar un data.frame como tabla HTML estilizada (pastel)
tabla_a_html <- function(df, html_path, titulo) {
  th <- paste0("<th style='background:#AED6F1;color:#1B2631;padding:8px 12px;",
               "border:1px solid #D5DBDB;'>", names(df), "</th>", collapse="")
  filas <- apply(df, 1, function(x)
    paste0("<tr>", paste0(
      "<td style='padding:6px 12px;border:1px solid #E5E8E8;'>",
      as.character(x), "</td>", collapse=""), "</tr>"))
  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><title>", titulo,
    "</title></head><body style='font-family:Segoe UI,Arial,sans-serif;",
    "background:#FDFEFE;padding:24px;'>",
    "<h2 style='color:#5D6D7E;'>", titulo, "</h2>",
    "<table style='border-collapse:collapse;font-size:13px;color:#2C3E50;'>",
    "<thead><tr>", th, "</tr></thead><tbody>",
    paste(filas, collapse=""), "</tbody></table></body></html>")
  writeLines(html, html_path, useBytes=TRUE)
}

# Texto Likert → número 1-5
likert_num <- function(x) {
  z <- trimws(as.character(x))
  z_ascii <- tolower(iconv(z, to="ASCII//TRANSLIT"))
  num <- suppressWarnings(as.integer(z))
  out <- dplyr::case_when(
    !is.na(num) & num %in% 1:5 ~ num,
    z_ascii == "totalmente en desacuerdo" ~ 1L,
    z_ascii == "en desacuerdo" ~ 2L,
    z_ascii == "neutral" ~ 3L,
    z_ascii == "de acuerdo" ~ 4L,
    z_ascii == "totalmente de acuerdo" ~ 5L,
    z_ascii == "strongly disagree" ~ 1L,
    z_ascii == "disagree" ~ 2L,
    z_ascii == "agree" ~ 4L,
    z_ascii == "strongly agree" ~ 5L,
    TRUE ~ NA_integer_
  )
  as.integer(out)
}

# Empresa → sector (para unir con industria del CSV)
empresa_a_sector <- function(emp) {
  e <- tolower(trimws(emp))
  dplyr::case_when(
    str_detect(e,"viera|hitss|indrive|nortal|on empresa|indra|xpertia|medilook|flip|kiro") ~ "Technology",
    str_detect(e,"ministerio|fiscalia|gobierno|public")   ~ "Government",
    str_detect(e,"bcp|bbva|banco|financ|inversion")       ~ "Finance",
    str_detect(e,"universid|educacion|escuela")            ~ "Education",
    str_detect(e,"apuesta|ramirez|manufactur")             ~ "Manufacturing",
    str_detect(e,"salud|clinic|hospital")                  ~ "Healthcare",
    TRUE                                                   ~ NA_character_
  )
}

# ── 1B. CONTRATO DE DATOS Y VALIDACIÓN ───────────────────────
TECH_REQUIRED <- c(
  "attack_type","target_system","outcome","timestamp",
  "data_compromised_GB","attack_duration_min","security_tools_used",
  "user_role","location","attack_severity","industry",
  "response_time_min","mitigation_method"
)

TECH_OPTIONAL_META <- c(
  "organization_id","business_unit","site","data_source",
  "record_origin","incident_id"
)

QUESTIONNAIRE_COLUMNS <- c(
  "timestamp","email","consent","organization_id","data_source",
  "gender","organization_or_company","job_role","business_unit",
  "experience","participates_security",
  "FI_1","FI_2","FI_3","FI_4","respondent_type","SI_1",
  "site","country","industry","incident_context","IE_1","RD_1",
  "policy_scope","years_in_organization","external_expert_organization",
  "PG_1","metadata_28","metadata_29","metadata_30","metadata_31",
  "metadata_32","metadata_33",
  "open_effective_policies","open_recommended_improvements",
  "open_main_difficulties","open_less_effective_policies",
  "SI_2","SI_3","SI_4","IE_2","IE_3","IE_4",
  "RD_2","RD_3","RD_4","PG_2","PG_3","PG_4"
)

plantilla_cuestionario <- function() {
  x <- as.data.frame(matrix(nrow=0, ncol=49))
  names(x) <- QUESTIONNAIRE_COLUMNS
  x
}

diccionario_tecnico <- function() {
  tibble::tribble(
    ~Field, ~Type, ~Required, ~Example, ~Expected_rule, ~Description,
    "attack_type","categorical","Yes","Phishing","non-empty","Type of cyberattack",
    "target_system","categorical","Yes","ERP","non-empty","Affected system",
    "outcome","categorical","Yes","Success","Success/Failure","Attack outcome",
    "timestamp","datetime","Yes","2026-01-15 10:30:00","YYYY-MM-DD HH:MM:SS","Incident date/time",
    "data_compromised_GB","numeric","Yes","2.5",">= 0","Compromised data volume in GB",
    "attack_duration_min","numeric","Yes","45",">= 0","Duration in minutes",
    "security_tools_used","categorical","Yes","EDR","non-empty","Security tool(s) involved",
    "user_role","categorical","Yes","Analyst","non-empty","User/actor role",
    "location","categorical","Yes","Lima","non-empty","Incident location",
    "attack_severity","numeric","Yes","8","1-10","Severity score",
    "industry","categorical","Yes","Finance","non-empty","Economic sector",
    "response_time_min","numeric","Yes","20",">= 0","Response time in minutes",
    "mitigation_method","categorical","Yes","Isolation","non-empty","Mitigation action",
    "organization_id","categorical","No","ORG_001","anonymized","Anonymous organization identifier",
    "business_unit","categorical","No","Operations","non-empty","Business unit",
    "site","categorical","No","HQ-Lima","non-empty","Site or facility",
    "data_source","categorical","No","SIEM","non-empty","Source system",
    "record_origin","categorical","No","Internal","non-empty","Record provenance",
    "incident_id","categorical","No","INC_0001","unique preferred","Incident traceability identifier"
  )
}

validar_incidentes <- function(df) {
  if (is.null(df) || nrow(df) == 0)
    return(list(ok=FALSE, errors="The technical incident file is empty.",
                warnings=character(), summary=tibble()))

  nms <- names(df)
  nms_low <- tolower(nms)
  req_low <- tolower(TECH_REQUIRED)
  faltan <- TECH_REQUIRED[!req_low %in% nms_low]
  errors <- character()
  warnings <- character()

  if (length(faltan))
    errors <- c(errors, paste("Missing required columns:", paste(faltan, collapse=", ")))

  if (length(errors) == 0) {
    getcol <- function(n) df[[which(nms_low == tolower(n))[1]]]
    num_rules <- list(
      data_compromised_GB=c(0, Inf),
      attack_duration_min=c(0, Inf),
      attack_severity=c(1, 10),
      response_time_min=c(0, Inf)
    )
    for (nm in names(num_rules)) {
      x0 <- getcol(nm)
      x <- suppressWarnings(as.numeric(x0))
      bad_type <- sum(!is.na(x0) & is.na(x))
      if (bad_type > 0)
        errors <- c(errors, sprintf("%s: %d non-numeric value(s).", nm, bad_type))
      lo <- num_rules[[nm]][1]; hi <- num_rules[[nm]][2]
      bad_rng <- sum(!is.na(x) & (x < lo | x > hi))
      if (bad_rng > 0)
        errors <- c(errors, sprintf("%s: %d value(s) outside expected range [%s, %s].",
                                    nm, bad_rng, lo, ifelse(is.infinite(hi),"Inf",hi)))
    }

    ts <- suppressWarnings(lubridate::ymd_hms(getcol("timestamp"), quiet=TRUE))
    bad_ts <- sum(!is.na(getcol("timestamp")) & is.na(ts))
    if (bad_ts > 0)
      errors <- c(errors, sprintf("timestamp: %d invalid date/time value(s); expected YYYY-MM-DD HH:MM:SS.", bad_ts))

    outc <- trimws(as.character(getcol("outcome")))
    unk_out <- setdiff(unique(outc[!is.na(outc) & nzchar(outc)]), c("Success","Failure"))
    if (length(unk_out))
      errors <- c(errors, paste("outcome contains unknown categories:",
                                paste(head(unk_out, 10), collapse=", ")))

    cats <- c("attack_type","target_system","security_tools_used","user_role",
              "location","industry","mitigation_method")
    for (nm in cats) {
      x <- trimws(as.character(getcol(nm)))
      if (any(!is.na(x) & !nzchar(x)))
        warnings <- c(warnings, paste(nm, "contains blank categories."))
    }
  }

  miss_tbl <- tibble::tibble(
    column=names(df),
    missing_n=vapply(df, function(x) sum(is.na(x) | (is.character(x) & trimws(x)=="")), numeric(1)),
    missing_pct=round(100*vapply(df, function(x) mean(is.na(x) | (is.character(x) & trimws(x)=="")), numeric(1)),2)
  )
  high_miss <- miss_tbl %>% dplyr::filter(missing_pct > 20)
  if (nrow(high_miss))
    warnings <- c(warnings, paste("Columns with >20% missing:",
                                  paste(high_miss$column, collapse=", ")))

  list(ok=length(errors)==0, errors=unique(errors),
       warnings=unique(warnings), summary=miss_tbl)
}

detectar_modo <- function(df, solicitado="auto") {
  if (!solicitado %in% c("auto","single","multi")) solicitado <- "auto"
  if (solicitado != "auto") return(solicitado)
  nms <- tolower(names(df))
  if ("organization_id" %in% nms) {
    x <- df[[which(nms=="organization_id")[1]]]
    n_org <- dplyr::n_distinct(x[!is.na(x) & trimws(as.character(x))!=""])
    if (n_org == 1) return("single")
    if (n_org > 1) return("multi")
  }
  "multi"
}


# ── 1C. VALIDACIÓN DEL CUESTIONARIO OPCIONAL ─────────────────
# El cuestionario es opcional: si falla esta validación se omite únicamente
# el módulo organizacional; el Technical Core puede continuar.
validar_cuestionario <- function(df) {
  if (is.null(df))
    return(list(
      ok=TRUE, provided=FALSE, errors=character(), warnings=character(),
      summary=tibble::tibble(), valid_rows=logical(), n_valid=0L,
      n_excluded=0L, n_consented=0L
    ))

  errors <- character()
  warnings <- character()

  if (nrow(df) == 0)
    errors <- c(errors, "The questionnaire file is empty.")
  if (ncol(df) < 49)
    errors <- c(errors, paste0(
      "Questionnaire requires at least 49 columns; detected: ", ncol(df), "."))

  valid_rows <- rep(FALSE, nrow(df))
  n_valid <- 0L
  n_excluded <- 0L
  n_consented <- 0L

  if (ncol(df) >= 49 && nrow(df) > 0) {
    consent <- trimws(as.character(df[[3]]))
    consent_ascii <- tolower(iconv(consent, to="ASCII//TRANSLIT"))
    accepted <- consent_ascii %in% c(
      "si, acepto participar", "si acepto participar",
      "yes, i agree to participate", "yes", "accepted"
    )
    n_consented <- sum(accepted, na.rm=TRUE)

    if (n_consented == 0)
      errors <- c(errors,
        "No consenting responses were detected in the consent field.")

    # Posiciones exactas usadas posteriormente por procesar_kdd()
    item_pos <- c(12:15, 17, 38:40, 22, 41:43, 23, 44:46, 27, 47:49)

    allowed_text <- c(
      "totalmente en desacuerdo","en desacuerdo","neutral",
      "de acuerdo","totalmente de acuerdo",
      "strongly disagree","disagree","agree","strongly agree"
    )

    invalid_total <- 0L
    valid_counts <- rep(0L, nrow(df))

    for (pos in item_pos) {
      x0 <- df[[pos]]
      x_chr <- trimws(as.character(x0))
      x_ascii <- tolower(iconv(x_chr, to="ASCII//TRANSLIT"))
      x_num <- suppressWarnings(as.integer(x_chr))

      is_valid_answer <- (!is.na(x_num) & x_num %in% 1:5) |
                         x_ascii %in% allowed_text
      is_blank <- is.na(x0) | x_chr == ""

      invalid_total <- invalid_total + sum(!is_blank & !is_valid_answer, na.rm=TRUE)
      valid_counts <- valid_counts + as.integer(!is_blank & is_valid_answer)
    }

    # Una fila válida requiere consentimiento + al menos 12/20 ítems válidos.
    valid_rows <- accepted & valid_counts >= 12
    n_valid <- sum(valid_rows, na.rm=TRUE)
    n_excluded <- sum(accepted & !valid_rows, na.rm=TRUE)

    # Valores Likert fuera de escala se reportan, pero no bloquean si aún
    # quedan respuestas utilizables.
    if (invalid_total > 0)
      warnings <- c(warnings, paste0(
        invalid_total,
        " Likert value(s) were outside the permitted 1–5 scale/labels and will be treated as missing."))

    # Respuestas incompletas no invalidan el cuestionario completo.
    if (n_excluded > 0)
      warnings <- c(warnings, paste0(
        n_excluded, " consenting respondent(s) answered fewer than 12 of 20 Likert items ",
        "and will be excluded; the remaining valid responses will still be analyzed."))

    n_partial <- sum(valid_rows & valid_counts < 20, na.rm=TRUE)
    if (n_partial > 0)
      warnings <- c(warnings, paste0(
        n_partial, " valid respondent(s) have 12–19 answered Likert items; ",
        "dimension scores will use available valid responses."))

    # Solo bloquea el módulo organizacional si no queda ninguna fila válida.
    if (n_consented > 0 && n_valid == 0)
      errors <- c(errors,
        "No valid consenting respondents remain after questionnaire completeness checks.")

    summary <- tibble::tibble(
      metric=c(
        "Rows detected",
        "Columns detected",
        "Consenting responses",
        "Valid responses retained",
        "Incomplete consenting responses excluded",
        "Valid responses with all 20 Likert items",
        "Out-of-scale Likert values"
      ),
      value=as.character(c(
        nrow(df), ncol(df), n_consented, n_valid, n_excluded,
        sum(valid_rows & valid_counts == 20, na.rm=TRUE),
        invalid_total
      ))
    )
  } else {
    summary <- tibble::tibble(
      metric=c("Rows detected","Columns detected"),
      value=as.character(c(nrow(df), ncol(df)))
    )
  }

  list(
    ok=length(errors)==0,
    provided=TRUE,
    errors=unique(errors),
    warnings=unique(warnings),
    summary=summary,
    valid_rows=valid_rows,
    n_valid=n_valid,
    n_excluded=n_excluded,
    n_consented=n_consented
  )
}

# ── 1D. DIAGNÓSTICO REPRODUCIBLE DE k ───────────────────────
diagnostico_k <- function(vars_num_df, k_max=8L,
                          sample_n=400L, seed=RANDOM_SEED) {
  x <- scale(as.data.frame(vars_num_df))
  n <- nrow(x)
  k_max <- min(as.integer(k_max), n - 1L)

  if (n < 3 || k_max < 2)
    return(tibble::tibble(
      k=integer(), WSS=numeric(), silhouette_mean=numeric(),
      n_silhouette=integer()
    ))

  # Mismo subconjunto para todos los k: comparación reproducible.
  set.seed(seed)
  idx <- sample(seq_len(n), min(sample_n, n))
  x_s <- x[idx,,drop=FALSE]

  rows <- lapply(seq_len(k_max), function(k) {
    set.seed(seed + k)
    km_full <- stats::kmeans(x, centers=k, nstart=25)
    sil_mean <- NA_real_

    if (k >= 2 && nrow(x_s) > k) {
      set.seed(seed + 1000L + k)
      km_s <- stats::kmeans(x_s, centers=k, nstart=25)
      sil_obj <- cluster::silhouette(km_s$cluster, stats::dist(x_s))
      sil_mean <- mean(sil_obj[,3], na.rm=TRUE)
    }

    tibble::tibble(
      k=k,
      WSS=km_full$tot.withinss,
      silhouette_mean=sil_mean,
      n_silhouette=ifelse(k >= 2, nrow(x_s), NA_integer_)
    )
  })

  dplyr::bind_rows(rows)
}

# ── 1E. PERFIL TÉCNICO DE CLUSTERS ───────────────────────────
caracterizar_clusters <- function(base) {
  if (is.null(base) || !"cluster" %in% names(base))
    return(tibble::tibble())

  base %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n_grupos             = dplyr::n(),
      frecuencia_total     = sum(n_incidentes, na.rm=TRUE),
      frecuencia_media     = mean(n_incidentes, na.rm=TRUE),
      impacto_total        = sum(impacto_total, na.rm=TRUE),
      impacto_medio        = mean(impacto_total, na.rm=TRUE),
      severidad_media      = mean(severidad_media, na.rm=TRUE),
      t_respuesta_media    = mean(t_respuesta_media, na.rm=TRUE),
      duracion_media       = mean(duracion_media, na.rm=TRUE),
      tasa_exito_media     = mean(tasa_exito, na.rm=TRUE),
      resiliencia_media    = mean(idx_resiliencia, na.rm=TRUE),
      .groups="drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~round(.x, 3))
    )
}

# ── 1F. RECOMENDACIONES TÉCNICAS PARA EL REPORTE ─────────────
recomendaciones_framework <- function(r) {
  out <- character()

  if (!is.null(r$cluster_profiles) && nrow(r$cluster_profiles) > 0) {
    cmax <- r$cluster_profiles %>%
      dplyr::arrange(dplyr::desc(impacto_total)) %>% dplyr::slice(1)
    out <- c(out, paste0(
      "Prioritize technical review of Cluster ", cmax$cluster,
      ", which shows the highest aggregated impact among the retained profiles."))
  }

  if (!is.null(r$dispersion_diag$status)) {
    out <- c(out, paste0(
      "Count-model inference follows the dispersion diagnosis: ",
      r$dispersion_diag$status, " (selected model: ", r$tipo_mdl, ")."))
  }

  if (isTRUE(r$tiene_encuesta) && !is.null(r$scores_enc)) {
    out <- c(out,
      "Interpret FI/SI/IE/RD/PG/EIglobal only as complementary organizational evidence; they did not enter PCA, K-Means or GLM.")
  } else {
    out <- c(out,
      "Optional Organizational Module not available; no organizational scores were inferred from technical incident data.")
  }

  tibble::tibble(recommendation=out)
}

# ── 2. FUNCIÓN KDD ───────────────────────────────────────────
# ── [v7.3.3] ANÁLISIS DE SENSIBILIDAD (revisión por pares) ──────
# Todo lo que pidieron los revisores queda dentro de la app:
#  (1) impacto total vs impacto medio (dependencia mecánica con el conteo),
#  (2) estabilidad de los terciles de frecuencia ajustada entre modelos,
#  (3) PCA sin variables derivadas + congruencia de Tucker,
#  (4) estabilidad de la silueta (100 submuestras + todas las celdas),
#  (5) Conway–Maxwell–Poisson (bajo demanda: botón en la pestaña
#      "Sensibilidad" o reproducir_articulo(..., cmp = TRUE)).
SENS_N_REP <- 100L   # submuestras para la estabilidad de la silueta
SENS_N_SUB <- 400L   # tamaño de cada submuestra (igual que en el artículo)

terciles_q <- function(x) {
  q <- stats::quantile(x, c(0, 0.33, 0.66, 1), na.rm=TRUE)
  if (length(unique(q)) == 4)
    cut(x, q, labels=c("Bajo","Medio","Alto"), include.lowest=TRUE)
  else
    cut(rank(x, ties.method="average")/sum(!is.na(x)),
        c(-Inf, 0.33, 0.66, Inf), labels=c("Bajo","Medio","Alto"))
}

tucker_cc <- function(a, b) abs(sum(a*b))/sqrt(sum(a^2)*sum(b^2))

analisis_sensibilidad <- function(base, base_res, mdl_formula, vars_num_df,
                                  km_cluster = NULL, sil_sel = NA_real_,
                                  sil_full = NA_real_,
                                  n_rep = SENS_N_REP, n_sub = SENS_N_SUB) {
  disp <- function(m) {
    x2 <- sum(stats::residuals(m, type="pearson")^2, na.rm=TRUE)
    df <- stats::df.residual(m)
    list(phi=x2/df, p_under=stats::pchisq(x2, df, lower.tail=TRUE),
         p_over=stats::pchisq(x2, df, lower.tail=FALSE))
  }

  # (1) Modelo con impacto medio, misma regla de dispersión que el principal
  base$log_impacto_medio <- log1p(base$impacto_medio)
  f_medio <- stats::update(mdl_formula, . ~ . - log_impacto + log_impacto_medio)
  mp <- stats::glm(f_medio, family=stats::poisson(link="log"), data=base)
  d  <- disp(mp)
  if (d$phi > 1 && d$p_over < 0.05) {
    m_med <- tryCatch(MASS::glm.nb(f_medio, data=base), error=function(e) mp)
    tipo  <- if (inherits(m_med, "negbin")) "Negative binomial" else "Poisson"
  } else if (d$phi < 1 && d$p_under < 0.05) {
    m_med <- stats::glm(f_medio, family=stats::quasipoisson(link="log"), data=base)
    tipo  <- "Quasi-Poisson"
  } else { m_med <- mp; tipo <- "Poisson" }

  y       <- base$n_incidentes
  fit_tot <- as.numeric(base_res$ajustados)
  fit_med <- as.numeric(stats::predict(m_med, type="response"))
  r2 <- function(o, f) stats::cor(o, f, use="complete.obs")^2

  resumen_impacto <- data.frame(
    specification = c("Total impact (main model)", "Mean impact (sensitivity)"),
    model         = c(NA, tipo),
    cor_frequency_impact = c(stats::cor(y, base$impacto_total),
                             stats::cor(y, base$impacto_medio)),
    r2_in_sample  = c(r2(y, fit_tot), r2(y, fit_med)),
    mae           = c(mean(abs(y - fit_tot)), mean(abs(y - fit_med))),
    poisson_dispersion_ratio_mean_model = c(NA, d$phi),
    stringsAsFactors = FALSE)
  coef_medio <- as.data.frame(broom::tidy(m_med, exponentiate=TRUE, conf.int=TRUE))

  # (2) Estabilidad de los terciles de prioridad
  cat_tot <- base_res$cat_frec_ajustada
  cat_med <- terciles_q(fit_med)
  tab <- table(Total_impact=cat_tot, Mean_impact=cat_med)
  n  <- sum(tab); po <- sum(diag(tab))/n
  pe <- sum(rowSums(tab) * colSums(tab))/n^2
  estabilidad <- data.frame(
    spearman_rho        = stats::cor(fit_tot, fit_med, method="spearman"),
    pct_same_tertile    = 100 * po,
    cohen_kappa         = (po - pe)/(1 - pe),
    pct_high_stays_high = 100 * tab["Alto","Alto"]/sum(tab["Alto", ]))
  tabla_terciles <- as.data.frame.matrix(tab)
  tabla_terciles <- cbind(Total_impact_tier=rownames(tabla_terciles), tabla_terciles)

  # (3) PCA sin variables derivadas + congruencia de Tucker
  X <- as.data.frame(vars_num_df)
  X <- X[, vapply(X, function(v) stats::sd(v, na.rm=TRUE) > 1e-8, logical(1)), drop=FALSE]
  variantes <- list(
    A_original_7vars          = names(X),
    B_sin_resiliencia         = setdiff(names(X), "idx_resiliencia"),
    C_sin_resiliencia_impacto = setdiff(names(X), c("idx_resiliencia","impacto_total")))
  pcas <- lapply(variantes, function(v) stats::prcomp(X[, v, drop=FALSE], center=TRUE, scale.=TRUE))
  pca_var <- do.call(rbind, lapply(names(pcas), function(nm) {
    ev <- pcas[[nm]]$sdev^2
    data.frame(variante=nm, PC=paste0("PC", seq_along(ev)), eigen=ev,
               var_pct=100*ev/sum(ev), var_acum=cumsum(100*ev/sum(ev)),
               stringsAsFactors=FALSE)
  }))
  pca_cargas <- do.call(rbind, lapply(names(pcas), function(nm) {
    rot <- pcas[[nm]]$rotation
    data.frame(variante=nm, variable=rownames(rot), PC1=rot[, 1], PC2=rot[, 2],
               stringsAsFactors=FALSE)
  }))
  rotA <- pcas$A_original_7vars$rotation
  tucker <- do.call(rbind, lapply(names(pcas)[-1], function(nm) {
    rot <- pcas[[nm]]$rotation; cm <- intersect(rownames(rotA), rownames(rot))
    data.frame(comparison=c(paste("Original PC1 vs", nm, "PC1"), paste("Original PC2 vs", nm, "PC2"),
                            paste("Original PC1 vs", nm, "PC2"), paste("Original PC2 vs", nm, "PC1")),
               tucker=c(tucker_cc(rotA[cm,1], rot[cm,1]), tucker_cc(rotA[cm,2], rot[cm,2]),
                        tucker_cc(rotA[cm,1], rot[cm,2]), tucker_cc(rotA[cm,2], rot[cm,1])),
               stringsAsFactors=FALSE)
  }))

  # (4) Estabilidad de la silueta
  Xs <- scale(X)
  sil_a <- sil_b <- numeric(0)
  if (!is.null(km_cluster) && length(km_cluster) == nrow(Xs) && nrow(Xs) > n_sub) {
    sil_a <- vapply(seq_len(n_rep), function(i) {
      set.seed(RANDOM_SEED + i)
      idx <- sample(nrow(Xs), n_sub)
      km  <- stats::kmeans(Xs[idx, ], centers=K_FINAL, nstart=25)
      mean(cluster::silhouette(km$cluster, stats::dist(Xs[idx, ]))[, 3])
    }, numeric(1))
    sil_b <- vapply(seq_len(n_rep), function(i) {
      set.seed(RANDOM_SEED + 5000 + i)
      idx <- sample(nrow(Xs), n_sub)
      mean(cluster::silhouette(km_cluster[idx], stats::dist(Xs[idx, ]))[, 3])
    }, numeric(1))
  }
  resumir <- function(x, nombre) data.frame(
    procedure=nombre, n=length(x), mean=if (length(x)) mean(x) else NA_real_,
    sd=if (length(x) > 1) stats::sd(x) else NA_real_,
    p2.5=if (length(x)) stats::quantile(x, .025) else NA_real_,
    p97.5=if (length(x)) stats::quantile(x, .975) else NA_real_,
    min=if (length(x)) min(x) else NA_real_, max=if (length(x)) max(x) else NA_real_,
    row.names=NULL, stringsAsFactors=FALSE)
  silueta <- rbind(
    resumir(sil_a, "a) Re-clustering of 400-cell subsamples (selection procedure)"),
    resumir(sil_b, "b) Final partition evaluated on 400-cell subsamples"),
    resumir(sil_full, "c) Final partition, all cells"),
    resumir(sil_sel, "Value used to select k (single subsample)"))

  out <- list(resumen_impacto=resumen_impacto, coef_impacto_medio=coef_medio,
              estabilidad_terciles=estabilidad, tabla_terciles=tabla_terciles, tab=tab,
              pca_var=pca_var, pca_cargas=pca_cargas, tucker=tucker, silueta=silueta,
              y=y, fit_tot=fit_tot, fit_med=fit_med, sil_a=sil_a, sil_b=sil_b,
              sil_full=sil_full, sil_sel=sil_sel)
  exportar_sensibilidad(out)
  out
}

# Conway–Maxwell–Poisson (lento con miles de celdas: se ejecuta bajo demanda)
ejecutar_cmp <- function(base, mdl_formula, mdl_final) {
  if (!requireNamespace("glmmTMB", quietly=TRUE))
    stop("Package 'glmmTMB' is required: install.packages('glmmTMB')")
  base$anio_c <- base$anio - min(base$anio, na.rm=TRUE)
  f_c <- if ("anio" %in% all.vars(mdl_formula))
    stats::update(mdl_formula, . ~ . - anio + anio_c) else mdl_formula
  m_pois <- stats::glm(f_c, family=stats::poisson(link="log"), data=base)
  b_ini  <- stats::coef(m_pois)
  ini    <- if (anyNA(b_ini)) NULL else list(beta=unname(b_ini))
  ctrl   <- glmmTMB::glmmTMBControl(parallel=max(1L, parallel::detectCores() - 1L),
                                    optCtrl=list(iter.max=300, eval.max=400))
  familia <- "Conway-Maxwell-Poisson"
  m <- tryCatch(glmmTMB::glmmTMB(f_c, family=glmmTMB::compois(), data=base,
                                 start=ini, control=ctrl), error=function(e) NULL)
  if (is.null(m)) {
    familia <- "Generalized Poisson (fallback)"
    m <- glmmTMB::glmmTMB(f_c, family=glmmTMB::genpois(), data=base, start=ini, control=ctrl)
  }
  cc <- summary(m)$coefficients$cond
  coef <- data.frame(term=rownames(cc), IRR=exp(cc[, 1]), SE_log=cc[, 2],
                     ci_low=exp(cc[, 1] - 1.96*cc[, 2]), ci_high=exp(cc[, 1] + 1.96*cc[, 2]),
                     p=cc[, 4], row.names=NULL, stringsAsFactors=FALSE)
  main <- as.data.frame(broom::tidy(mdl_final, exponentiate=TRUE))
  main$term <- sub("^anio$", "anio_c", main$term)
  comp <- merge(main[, c("term","estimate","p.value")], coef[, c("term","IRR","p")], by="term")
  comp <- comp[comp$term != "(Intercept)", ]
  names(comp) <- c("term","IRR_main","p_main","IRR_alt","p_alt")
  comp$same_direction    <- sign(log(comp$IRR_main)) == sign(log(comp$IRR_alt))
  comp$same_significance <- (comp$p_main < .05) == (comp$p_alt < .05)
  resumen <- data.frame(
    model=c("Poisson (reference)", familia),
    AIC=c(stats::AIC(m_pois), stats::AIC(m)), BIC=c(stats::BIC(m_pois), stats::BIC(m)),
    dispersion_parameter=c(NA, stats::sigma(m)),
    spearman_fitted_vs_main=c(NA, stats::cor(stats::fitted(mdl_final),
                                             stats::fitted(m), method="spearman")),
    pct_same_direction=c(NA, 100*mean(comp$same_direction)),
    pct_same_significance=c(NA, 100*mean(comp$same_significance)),
    stringsAsFactors=FALSE)
  out <- list(resumen=resumen, coef=coef, comparacion=comp)
  dir_crear("outputs/tablas")
  tryCatch({
    readr::write_csv(resumen, "outputs/tablas/28_sens_com_poisson_summary.csv")
    readr::write_csv(comp,    "outputs/tablas/29_sens_com_poisson_vs_main.csv")
  }, error=function(e) NULL)
  out
}

exportar_sensibilidad <- function(s) {
  dir_crear("outputs/tablas")
  tryCatch({
    readr::write_csv(s$resumen_impacto,      "outputs/tablas/20_sens_impact_total_vs_mean.csv")
    readr::write_csv(s$coef_impacto_medio,   "outputs/tablas/21_sens_mean_impact_coefficients.csv")
    readr::write_csv(s$estabilidad_terciles, "outputs/tablas/22_sens_tier_stability.csv")
    readr::write_csv(s$tabla_terciles,       "outputs/tablas/23_sens_tier_crosstab.csv")
    readr::write_csv(s$pca_var,              "outputs/tablas/24_sens_pca_variance.csv")
    readr::write_csv(s$pca_cargas,           "outputs/tablas/25_sens_pca_loadings.csv")
    readr::write_csv(s$tucker,               "outputs/tablas/26_sens_pca_tucker.csv")
    readr::write_csv(s$silueta,              "outputs/tablas/27_sens_silhouette_stability.csv")
  }, error=function(e) NULL)
  invisible(TRUE)
}

# ── [v7.3.3] FIGURA 11 DEL ARTÍCULO: ANÁLISIS DE SENSIBILIDAD ─
FIG11_PAL <- list(navy="#2D6E95", red="#D1495B", teal="#2A9D8F", amber="#EDAE49",
                  ink="#1F2D3D", grey="#6B7785", grid="#E6E9ED")

theme_fig11 <- function(base_size = 10.5) {
  P <- FIG11_PAL
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face="bold", size=base_size + 2, colour=P$ink,
                                            margin=ggplot2::margin(b=2)),
      plot.subtitle = ggplot2::element_text(size=base_size - 0.5, colour=P$grey,
                                            margin=ggplot2::margin(b=6)),
      plot.title.position = "plot",
      axis.title    = ggplot2::element_text(size=base_size, colour=P$ink),
      axis.text     = ggplot2::element_text(size=base_size - 1.5, colour=P$grey),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour=P$grid, linewidth=0.35),
      strip.text    = ggplot2::element_text(face="bold", size=base_size, colour=P$ink, hjust=0),
      legend.position = "bottom",
      legend.title  = ggplot2::element_blank(),
      legend.text   = ggplot2::element_text(size=base_size - 1, colour=P$ink),
      plot.margin   = ggplot2::margin(6, 10, 6, 6))
}

plot_figure11 <- function(s) {
  P <- FIG11_PAL
  tiers_en <- c(Bajo="Low", Medio="Medium", Alto="High")

  # A: observados vs ajustados
  mae <- function(o, f) mean(abs(o - f))
  lab_tot <- "Total impact (main model)"; lab_med <- "Mean impact (sensitivity)"
  dA <- rbind(data.frame(model=lab_tot, observed=s$y, fitted=s$fit_tot),
              data.frame(model=lab_med, observed=s$y, fitted=s$fit_med))
  dA$model <- factor(dA$model, levels=c(lab_tot, lab_med))
  ann <- data.frame(model=factor(c(lab_tot, lab_med), levels=c(lab_tot, lab_med)),
    txt=c(sprintf("r² = %.3f\nMAE = %.2f", stats::cor(s$y, s$fit_tot)^2, mae(s$y, s$fit_tot)),
          sprintf("r² = %.3f\nMAE = %.2f", stats::cor(s$y, s$fit_med)^2, mae(s$y, s$fit_med))))
  lim <- range(c(dA$observed, dA$fitted), na.rm=TRUE)
  set.seed(RANDOM_SEED)
  pA <- ggplot2::ggplot(dA, ggplot2::aes(observed, fitted)) +
    ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed", colour=P$red, linewidth=0.6) +
    ggplot2::geom_jitter(width=0.28, height=0, size=0.5, alpha=0.18, colour=P$navy) +
    ggplot2::geom_label(data=ann, ggplot2::aes(x=lim[1], y=lim[2], label=txt), inherit.aes=FALSE,
                        hjust=0, vjust=1, size=3.3, label.size=0, fill="white",
                        colour=P$ink, lineheight=1.05, fontface="bold") +
    ggplot2::facet_wrap(~ model, nrow=1) +
    ggplot2::coord_cartesian(xlim=lim, ylim=lim) +
    ggplot2::labs(title="A   Observed vs. fitted incident frequency",
      subtitle="The close fit of the main model disappears when impact is not summed over the counted incidents",
      x="Observed incidents per cell", y="Fitted incidents per cell") +
    theme_fig11()

  # B: flujo de terciles de prioridad [v7.3.3]
  tab <- as.matrix(s$tab)
  N <- sum(tab); po <- sum(diag(tab))/N; pe <- sum(rowSums(tab)*colSums(tab))/N^2
  pB <- plot_tier_flow(tab, title="B   Priority tier flow",
    subtitle=sprintf("Same tier: %.1f%% of cells\nCohen's \u03BA = %.2f  \u00B7  Spearman \u03C1 = %.2f\n ",
                     100*po, (po - pe)/(1 - pe), stats::cor(s$fit_tot, s$fit_med, method="spearman")),
    left_lab="Total impact", right_lab="Mean impact", xpad=0.45, label_size=2.8)

  # C: estabilidad de la silueta
  if (length(s$sil_a) && length(s$sil_b)) {
    dC <- rbind(data.frame(proc="Re-clustering of subsamples (selection procedure)", v=s$sil_a),
                data.frame(proc="Final partition evaluated on subsamples", v=s$sil_b))
    dC$proc <- factor(dC$proc, levels=unique(dC$proc))
    vl <- data.frame(x=c(s$sil_sel, s$sil_full, 0.25),
                     lab=sprintf("%.3f", c(s$sil_sel, s$sil_full, 0.25)),
                     col=c(P$red, P$ink, P$grey), lt=c("solid", "longdash", "dotted"),
                     hj=c(-0.12, 1.12, 0.5))
    vl <- vl[is.finite(vl$x), , drop=FALSE]
    xr <- range(c(dC$v, vl$x), na.rm=TRUE) + c(-0.01, 0.01)
    pC <- ggplot2::ggplot(dC, ggplot2::aes(v, fill=proc, colour=proc)) +
      ggplot2::geom_density(alpha=0.35, linewidth=0.6, adjust=1.1) +
      ggplot2::geom_vline(xintercept=vl$x, colour=vl$col, linetype=vl$lt, linewidth=0.7) +
      ggplot2::annotate("text", x=vl$x, y=Inf, label=vl$lab, hjust=vl$hj, vjust=1.4,
                        size=2.9, colour=vl$col, fontface="bold") +
      ggplot2::scale_fill_manual(values=c(P$teal, P$navy)) +
      ggplot2::scale_colour_manual(values=c(P$teal, P$navy)) +
      ggplot2::scale_x_continuous(limits=xr, breaks=scales::pretty_breaks(5)) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=2), colour=ggplot2::guide_legend(nrow=2)) +
      ggplot2::labs(title="C   Silhouette stability",
        subtitle=sprintf("%d subsamples of %d cells: %.3f ± %.3f\nRed: used to select k · dashed: all cells\nDotted: 0.25 threshold for structure",
                         length(s$sil_a), SENS_N_SUB, mean(s$sil_a), stats::sd(s$sil_a)),
        x="Mean silhouette width", y="Density") +
      theme_fig11()
  } else {
    pC <- ggplot2::ggplot() + ggplot2::labs(title="C   Silhouette stability",
      subtitle="Not available (too few cells)") + theme_fig11()
  }

  # D: varianza por componente en las tres especificaciones del PCA
  spec_lab <- c(A_original_7vars="7 variables (original)",
                B_sin_resiliencia="Without resilience index",
                C_sin_resiliencia_impacto="Without resilience index and total impact")
  dD <- s$pca_var
  dD$spec <- factor(spec_lab[dD$variante], levels=spec_lab)
  dD$k <- as.integer(sub("PC", "", dD$PC))
  kmax <- max(dD$k)
  pD <- ggplot2::ggplot(dD, ggplot2::aes(k, var_pct, colour=spec)) +
    ggplot2::geom_hline(yintercept=20, linetype="dashed", colour=P$grey, linewidth=0.5) +
    ggplot2::annotate("text", x=kmax + 0.3, y=24, label="20% = equal share\n(5 variables)",
                      hjust=1, size=2.7, colour=P$grey, lineheight=0.9) +
    ggplot2::geom_line(linewidth=0.9) + ggplot2::geom_point(size=2.3) +
    ggplot2::annotate("text", x=kmax + 0.3, y=9, label=sprintf("PC%d ≈ 0%%:\nexact linear\ndependence", kmax),
                      hjust=1, size=2.7, colour=P$navy, lineheight=0.9) +
    ggplot2::scale_colour_manual(values=c(P$navy, P$teal, P$amber)) +
    ggplot2::scale_x_continuous(breaks=seq_len(kmax), labels=paste0("PC", seq_len(kmax)),
                                limits=c(0.8, kmax + 0.3)) +
    ggplot2::scale_y_continuous(labels=function(v) paste0(v, "%"), limits=c(0, NA)) +
    ggplot2::guides(colour=ggplot2::guide_legend(nrow=3)) +
    ggplot2::labs(title="D   Variance by component",
                  subtitle="Structure fades once derived\nindicators are removed\n ",
                  x=NULL, y="Explained variance") +
    theme_fig11()

  pA / ((pB | pC | pD) + patchwork::plot_layout(widths=c(1.25, 1, 1))) + patchwork::plot_layout(heights=c(1.05, 1))
}


# ── [v7.3.3] RESUMEN VISUAL: FLUJO DE TERCILES, SUPERFICIE DEL GLM Y RADAR ──
# Diagrama de flujo (tipo Sankey) dibujado solo con ggplot2: muestra cómo
# pasan las celdas entre los terciles de prioridad de dos especificaciones.
plot_tier_flow <- function(tab, title = "Priority tier flow",
                           subtitle = NULL, left_lab = "Total-impact model",
                           right_lab = "Mean-impact model", base_size = 10.5,
                           xpad = 0.22, label_size = 3.2) {
  P <- FIG11_PAL
  tiers <- c("Bajo", "Medio", "Alto")
  tiers_en <- c(Bajo = "Low", Medio = "Medium", Alto = "High")
  tab <- as.matrix(tab)[tiers, tiers]
  N <- sum(tab); gap <- 0.035 * N; w <- 0.055
  node_col <- c(Bajo = P$teal, Medio = P$amber, Alto = P$red)
  # node positions (top = High)
  pos <- function(tot) {
    o <- rev(tiers); y1 <- numeric(3); y0 <- numeric(3); top <- N + 2 * gap
    for (k in seq_along(o)) { y1[k] <- top; y0[k] <- top - tot[o[k]]; top <- y0[k] - gap }
    data.frame(tier = o, y0 = y0, y1 = y1)
  }
  L <- pos(rowSums(tab)); R <- pos(colSums(tab))
  # ribbons
  sig <- function(t) (1 - cos(pi * t)) / 2
  tt <- seq(0, 1, length.out = 60)
  lcur <- setNames(L$y1, L$tier); rcur <- setNames(R$y1, R$tier)
  rib <- list(); labs <- list(); id <- 0
  for (i in rev(tiers)) for (j in rev(tiers)) {
    n <- tab[i, j]; if (n <= 0) next
    id <- id + 1
    ly1 <- lcur[i]; ly0 <- ly1 - n; lcur[i] <- ly0
    ry1 <- rcur[j]; ry0 <- ry1 - n; rcur[j] <- ry0
    x <- w + (1 - 2 * w) * tt
    top <- ly1 + (ry1 - ly1) * sig(tt); bot <- ly0 + (ry0 - ly0) * sig(tt)
    rib[[id]] <- data.frame(id = id, from = i, keep = i == j,
                            x = c(x, rev(x)), y = c(top, rev(bot)))
    labs[[id]] <- data.frame(from = i, keep = i == j, x = 0.5,
                             y = (top[30] + bot[30]) / 2,
                             pct = 100 * n / sum(tab[i, ]), n = n)
  }
  rib <- do.call(rbind, rib); labs <- do.call(rbind, labs)
  nodes <- rbind(transform(L, x0 = 0, x1 = w, side = "L"),
                 transform(R, x0 = 1 - w, x1 = 1, side = "R"))
  nodes$n <- ifelse(nodes$side == "L", rowSums(tab)[nodes$tier], colSums(tab)[nodes$tier])
  nodes$lab <- paste0(tiers_en[nodes$tier], "\n", format(nodes$n, big.mark = ","))
  po <- sum(diag(tab)) / N; pe <- sum(rowSums(tab) * colSums(tab)) / N^2
  if (is.null(subtitle))
    subtitle <- sprintf("Cells that keep their tier: %.1f%%  ·  Cohen's κ = %.2f", 100 * po, (po - pe) / (1 - pe))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = rib, ggplot2::aes(x, y, group = id, fill = from, alpha = keep),
                          colour = "white", linewidth = 0.25) +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = tier),
                       colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = nodes[nodes$side == "L", ], ggplot2::aes(x = x0 - 0.015, y = (y0 + y1) / 2, label = lab),
                       hjust = 1, size = label_size, lineheight = 0.9, fontface = "bold", colour = P$ink) +
    ggplot2::geom_text(data = nodes[nodes$side == "R", ], ggplot2::aes(x = x1 + 0.015, y = (y0 + y1) / 2, label = lab),
                       hjust = 0, size = label_size, lineheight = 0.9, fontface = "bold", colour = P$ink) +
    ggplot2::geom_label(data = labs[labs$keep | labs$pct >= 30, ],
                        ggplot2::aes(x = x, y = y, label = sprintf("%.0f%%", pct)),
                        size = 2.9, label.size = 0, fill = "white", alpha = 0.85, colour = P$ink, fontface = "bold") +
    ggplot2::annotate("text", x = c(w / 2, 1 - w / 2), y = N + 2 * gap + 0.06 * N,
                      label = c(left_lab, right_lab), size = label_size + 0.1, fontface = "bold", colour = P$grey) +
    ggplot2::scale_fill_manual(values = node_col, guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 0.75, `FALSE` = 0.32), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-xpad, 1 + xpad), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_fig11(base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())
}

# Superficie de respuesta del GLM: frecuencia ajustada según log-impacto
# y severidad media, con las demás covariables en valores típicos.
plot_glm_surface <- function(r, n_grid = 90L, base_size = 10.5) {
  P <- FIG11_PAL
  m <- r$mdl_final; b <- r$base
  if (is.null(m) || !all(c("log_impacto", "severidad_media") %in% names(b))) return(NULL)
  vars <- setdiff(all.vars(r$mdl_formula)[-1], c("log_impacto", "severidad_media"))
  q <- function(v) stats::quantile(b[[v]], c(0.01, 0.99), na.rm = TRUE)
  g <- expand.grid(log_impacto = seq(q("log_impacto")[1], q("log_impacto")[2], length.out = n_grid),
                   severidad_media = seq(q("severidad_media")[1], q("severidad_media")[2], length.out = n_grid))
  for (v in vars) {
    x <- b[[v]]
    if (is.numeric(x)) g[[v]] <- stats::median(x, na.rm = TRUE)
    else { tb <- sort(table(x), decreasing = TRUE); g[[v]] <- factor(names(tb)[1], levels = levels(factor(x))) }
  }
  g$fit <- as.numeric(stats::predict(m, newdata = g, type = "response"))
  set.seed(RANDOM_SEED)
  pts <- b[sample(nrow(b), min(1500L, nrow(b))), c("log_impacto", "severidad_media")]
  pts <- pts[pts$log_impacto >= min(g$log_impacto) & pts$log_impacto <= max(g$log_impacto) &
             pts$severidad_media >= min(g$severidad_media) & pts$severidad_media <= max(g$severidad_media), ]
  ggplot2::ggplot(g, ggplot2::aes(log_impacto, severidad_media)) +
    ggplot2::geom_raster(ggplot2::aes(fill = fit), interpolate = TRUE) +
    ggplot2::geom_point(data = pts, colour = "white", alpha = 0.35, size = 0.45) +
    ggplot2::geom_contour(ggplot2::aes(z = fit), colour = "#F6E27F", linewidth = 0.55, bins = 9) +
    ggplot2::scale_fill_gradientn(colours = c("#0B2545", "#134074", "#13315C", "#1D7EA8", "#39A9DB", "#9BD7F0", "#E8F6FC"),
                                  name = "Fitted\nincidents\nper cell") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(title = "GLM response surface",
                  subtitle = "Fitted incident frequency by log impact and mean severity\n(other covariates at typical values)",
                  x = "Log total impact", y = "Mean severity") +
    theme_fig11(base_size) + ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_text(size = base_size - 1.5))
}

# Radar de perfiles K-Means: centroides estandarizados (z) de las 7 variables
RADAR_LABELS <- c(n_incidentes = "Incident\nfrequency", impacto_total = "Economic\nimpact",
                  severidad_media = "Mean\nseverity", t_respuesta_media = "Mean response\ntime",
                  duracion_media = "Mean attack\nduration", tasa_exito = "Attack\nsuccess rate",
                  idx_resiliencia = "Resilience\nindex")
plot_cluster_radar <- function(r, base_size = 10.5) {
  P <- FIG11_PAL
  b <- r$base; vc <- intersect(r$vars_cols, names(b))
  if (is.null(b$cluster) || length(vc) < 3) return(NULL)
  ok <- !is.na(b$cluster)
  X <- scale(as.data.frame(lapply(b[ok, vc, drop = FALSE], as.numeric)))
  cen <- stats::aggregate(as.data.frame(X), by = list(cluster = b$cluster[ok]), FUN = mean)
  k <- length(vc); ang <- pi / 2 - 2 * pi * (seq_len(k) - 1) / k
  zmin <- -1.5; zmax <- 1.5; rad <- function(z) 0.15 + 0.85 * (pmin(pmax(z, zmin), zmax) - zmin) / (zmax - zmin)
  poly <- do.call(rbind, lapply(seq_len(nrow(cen)), function(i) {
    z <- unlist(cen[i, vc]); rr <- rad(z)
    data.frame(cluster = paste0("Profile ", cen$cluster[i]), x = rr * cos(ang), y = rr * sin(ang), ord = seq_len(k))
  }))
  ring <- do.call(rbind, lapply(c(-1, 0, 1), function(z) {
    t <- seq(0, 2 * pi, length.out = 200); data.frame(z = z, x = rad(z) * cos(t), y = rad(z) * sin(t))
  }))
  spokes <- data.frame(x = 0, y = 0, xend = cos(ang), yend = sin(ang))
  labs <- data.frame(x = 1.17 * cos(ang), y = 1.17 * sin(ang), lab = unname(RADAR_LABELS[vc]))
  labs$lab[is.na(labs$lab)] <- vc[is.na(labs$lab)]
  ringlab <- data.frame(x = 0.02, y = rad(c(-1, 0, 1)), lab = c("z = −1", "z = 0", "z = +1"))
  pal <- c(P$navy, P$red, P$teal, P$amber, "#7B5EA7")
  ggplot2::ggplot() +
    ggplot2::geom_path(data = ring, ggplot2::aes(x, y, group = z), colour = P$grid,
                       linewidth = ifelse(ring$z == 0, 0.8, 0.4)) +
    ggplot2::geom_segment(data = spokes, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), colour = P$grid, linewidth = 0.4) +
    ggplot2::geom_polygon(data = poly, ggplot2::aes(x, y, group = cluster, fill = cluster, colour = cluster),
                          alpha = 0.18, linewidth = 0.9) +
    ggplot2::geom_point(data = poly, ggplot2::aes(x, y, colour = cluster), size = 2) +
    ggplot2::geom_text(data = labs, ggplot2::aes(x, y, label = lab), size = 2.9, lineheight = 0.85, colour = P$ink) +
    ggplot2::geom_text(data = ringlab, ggplot2::aes(x, y, label = lab), size = 2.5, hjust = 0, vjust = -0.3, colour = P$grey) +
    ggplot2::scale_fill_manual(values = pal) + ggplot2::scale_colour_manual(values = pal) +
    ggplot2::coord_equal(xlim = c(-1.35, 1.35), ylim = c(-1.3, 1.3)) +
    ggplot2::labs(title = "Exploratory technical profiles (K-Means)",
                  subtitle = sprintf("Standardized centroids; overlapping shapes\nreflect weak separation (silhouette = %.3f)",
                                     if (!is.null(r$sil_final_full) && is.finite(r$sil_final_full)) r$sil_final_full else NA)) +
    theme_fig11(base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank())
}

# Dashboard de la pestaña "Resumen visual"
plot_visual_summary <- function(r) {
  s <- r$sensibilidad
  pa <- plot_glm_surface(r); pr <- plot_cluster_radar(r)
  pf <- if (!is.null(s)) plot_tier_flow(s$tab, title = "Priority tier flow between model specifications") else NULL
  pa <- if (is.null(pa)) ggplot2::ggplot() + ggplot2::labs(title = "GLM response surface: not available") + theme_fig11() else pa
  pr <- if (is.null(pr)) ggplot2::ggplot() + ggplot2::labs(title = "Profiles: not available") + theme_fig11() else pr
  pa <- pa + ggplot2::labs(title = paste0("A   ", pa$labels$title))
  pr <- pr + ggplot2::labs(title = paste0("B   ", pr$labels$title))
  if (is.null(pf)) return(pa | pr)
  pf <- pf + ggplot2::labs(title = paste0("C   ", pf$labels$title))
  (pa | pr) / pf + patchwork::plot_layout(heights = c(1.1, 1))
}

# ── [v7.3.3] REPRODUCIR EL ARTÍCULO SIN ABRIR LA APP ────────────
# Uso: source("cybersecurity_framework_v7_4_1.R")
#      reproducir_articulo("data/cybersecurity_incidents.csv")
# Genera: outputs/revision/revision_resultados.xlsx (todos los análisis),
#         outputs/article_figures/ (Figuras 1–11 en PDF y JPG a 600 dpi).
reproducir_articulo <- function(csv_path, cmp = TRUE, figuras = TRUE) {
  raw <- readr::read_csv(csv_path, show_col_types=FALSE)
  val <- validar_incidentes(raw)
  if (!isTRUE(val$ok))
    stop("The CSV does not pass validation:\n- ", paste(val$errors, collapse="\n- "), call.=FALSE)
  message("Running KDD pipeline (framework v", FRAMEWORK_VERSION, ") ...")
  r <- procesar_kdd(raw, NULL)
  s <- r$sensibilidad
  hojas <- list(
    Sens_impact            = s$resumen_impacto,
    Sens_mean_impact_coef  = s$coef_impacto_medio,
    Sens_tier_stability    = s$estabilidad_terciles,
    Sens_tier_crosstab     = s$tabla_terciles,
    Sens_PCA_variance      = s$pca_var,
    Sens_PCA_loadings      = s$pca_cargas,
    Sens_PCA_tucker        = s$tucker,
    Sens_silhouette        = s$silueta,
    Main_model_coefficients= as.data.frame(r$tbl_coef),
    Session_info           = data.frame(session_info=utils::capture.output(utils::sessionInfo())))
  dir_crear("outputs/revision")
  out_xlsx <- "outputs/revision/revision_resultados.xlsx"
  writexl::write_xlsx(hojas, out_xlsx)
  writeLines(utils::capture.output(utils::sessionInfo()), "outputs/revision/sessionInfo.txt")
  if (isTRUE(figuras)) {
    message("Exporting article figures (including Figure 11) ...")
    print(generate_article_figures(r))
  }
  if (isTRUE(cmp)) {
    message("Fitting Conway-Maxwell-Poisson (this can take several minutes) ...")
    cm <- tryCatch(ejecutar_cmp(r$base, r$mdl_formula, r$mdl_final),
                   error=function(e) { message("COM-Poisson error: ", conditionMessage(e)); NULL })
    if (!is.null(cm)) {
      hojas$Sens_COM_Poisson_summary <- cm$resumen
      hojas$Sens_COM_Poisson_vs_main <- cm$comparacion
      writexl::write_xlsx(hojas, out_xlsx)
      r$cmp <- cm
    }
  }
  message("Done: ", normalizePath(out_xlsx, winslash="/"))
  invisible(r)
}

procesar_kdd <- function(raw_csv, raw_xlsx = NULL, analysis_mode = "auto") {
  modo_analisis <- detectar_modo(raw_csv, analysis_mode)

  ## =========================================================
  ## FASE 1 — SELECCIÓN
  ## =========================================================
  # Renombrar columnas CSV a nombres internos del modelo
  datos_orig <- raw_csv %>%
    rename_with(tolower) %>%         # todo minúscula
    rename(
      tipo_ataque    = attack_type,
      sistema        = target_system,
      resultado      = outcome,
      gb_comprometidos = data_compromised_gb,
      duracion_min   = attack_duration_min,
      herramienta    = security_tools_used,
      rol_usuario    = user_role,
      pais           = location,
      severidad      = attack_severity,
      industria      = industry,
      t_respuesta_min= response_time_min,
      mitigacion     = mitigation_method
    ) %>%
    mutate(
      fecha      = ymd_hms(timestamp, quiet=TRUE),
      anio       = year(fecha),
      mes        = month(fecha),
      # proxy impacto económico: GB × severidad × 1000
      impacto    = gb_comprometidos * severidad * 1000,
      exito      = as.integer(resultado == "Success")
    )

  tryCatch(write_csv(datos_orig, "outputs/tablas/01_seleccion.csv"),
           error=function(e) NULL)

  ## =========================================================
  ## FASE 2 — PREPROCESAMIENTO
  ## =========================================================
  vars_criticas <- c("tipo_ataque","pais","industria","impacto",
                     "severidad","t_respuesta_min","duracion_min","exito")

  tbl_faltantes <- datos_orig %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to="variable",
                 values_to="valores_faltantes") %>%
    filter(valores_faltantes > 0)

  n0 <- nrow(datos_orig)
  datos_limpios <- datos_orig %>%
    drop_na(all_of(vars_criticas)) %>%
    mutate(
      tipo_ataque = factor(tipo_ataque),
      sistema     = factor(sistema),
      resultado   = factor(resultado),
      pais        = factor(pais),
      industria   = factor(industria),
      herramienta = factor(herramienta),
      mitigacion  = factor(mitigacion),
      rol_usuario = factor(rol_usuario)
    )
  n1 <- nrow(datos_limpios)

  resumen_limpieza <- tibble(
    registros_originales = n0,
    registros_limpios    = n1,
    eliminados           = n0 - n1,
    pct_retencion        = round(n1/n0*100, 1)
  )

  tryCatch({
    write_csv(tbl_faltantes,    "outputs/tablas/02_faltantes.csv")
    write_csv(resumen_limpieza, "outputs/tablas/03_limpieza.csv")
  }, error=function(e) NULL)

  ## =========================================================
  ## FASE 3 — TRANSFORMACIÓN: base modelo (bloque TÉCNICO)
  ## =========================================================
  base <- datos_limpios %>%
    group_by(anio, mes, pais, industria, tipo_ataque) %>%
    summarise(
      n_incidentes         = n(),
      impacto_total        = sum(impacto, na.rm=TRUE),
      impacto_medio        = mean(impacto, na.rm=TRUE),
      severidad_media      = mean(severidad, na.rm=TRUE),
      duracion_media       = mean(duracion_min, na.rm=TRUE),
      t_respuesta_media    = mean(t_respuesta_min, na.rm=TRUE),
      tasa_exito           = mean(exito, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      log_impacto = log1p(impacto_total),

      # Índice de resiliencia: menor tiempo de respuesta → mayor resiliencia
      idx_resiliencia = 100 - scales::rescale(t_respuesta_media + duracion_media,
                                      to=c(0,100)),

      severidad_cat = cut(severidad_media,
                          breaks=c(0,3.3,6.6,10),
                          labels=c("Leve","Moderada","Crítica"),
                          include.lowest=TRUE),

      resiliencia_cat = case_when(
        idx_resiliencia >= 70 ~ "Alta",
        idx_resiliencia >= 40 ~ "Media",
        TRUE                  ~ "Baja"
      ) %>% factor(levels=c("Baja","Media","Alta"))
    )

  tryCatch(write_csv(base, "outputs/tablas/04_base_transformada.csv"),
           error=function(e) NULL)

  ## =========================================================
  ## FASE 3B — TRANSFORMACIÓN: encuesta Likert
  ##   [v7.3.2] Optional Organizational Module.
  ##   Las dimensiones se calculan sobre ítems redactados en
  ##   sentido favorable con escala 1-5 correctamente orientada,
  ##   pero se reportan aparte: NO alimentan PCA, K-Means ni GLM.
  ## =========================================================
  # Posiciones R (1-based) de cada dimensión:
  # FI: 12,13,14,15 | SI: 17,38,39,40
  # IE: 22,41,42,43 | RD: 23,44,45,46
  # PG: 27,47,48,49 | Abiertas: 34,35,36,37
  alpha_enc <- NULL; enc_limpia <- NULL
  scores_enc <- NULL; texto_abierto <- NULL; scores_sector <- NULL

  if (!is.null(raw_xlsx) && ncol(raw_xlsx) >= 49) {

    enc <- raw_xlsx

    # ── Renombrar por posición ────────────────────────────
    colnames(enc)[3]  <- "acepta"
    colnames(enc)[6]  <- "genero"
    colnames(enc)[7]  <- "empresa"
    colnames(enc)[8]  <- "cargo"
    colnames(enc)[9]  <- "area"
    colnames(enc)[10] <- "experiencia"
    colnames(enc)[11] <- "participa"

    # Metadatos opcionales del template reusable (si existen por nombre)
    nms_enc_low <- tolower(names(enc))
    if ("respondent_type" %in% nms_enc_low) {
      names(enc)[which(nms_enc_low=="respondent_type")[1]] <- "respondent_type"
    }

    colnames(enc)[12] <- "f1"  # incidentes han disminuido
    colnames(enc)[13] <- "f2"  # menor frecuencia de ataques
    colnames(enc)[14] <- "f3"  # políticas previenen repetición
    colnames(enc)[15] <- "f4"  # controles reducen vulnerabilidades

    colnames(enc)[17] <- "s1"  # reducción en gravedad
    colnames(enc)[38] <- "s2"  # impacto operativo disminuido
    colnames(enc)[39] <- "s3"  # incidentes menos críticos
    colnames(enc)[40] <- "s4"  # controles limitan consecuencias

    colnames(enc)[22] <- "i1"  # pérdidas económicas reducidas
    colnames(enc)[41] <- "i2"  # costos de recuperación bajos
    colnames(enc)[42] <- "i3"  # menor impacto financiero
    colnames(enc)[43] <- "i4"  # inversión reduce gastos

    colnames(enc)[23] <- "r1"  # respuesta rápida a incidentes
    colnames(enc)[44] <- "r2"  # tiempo de recuperación adecuado
    colnames(enc)[45] <- "r3"  # continuidad operativa
    colnames(enc)[46] <- "r4"  # equipo preparado

    colnames(enc)[27] <- "p1"  # políticas efectivas
    colnames(enc)[47] <- "p2"  # medidas fortalecen protección
    colnames(enc)[48] <- "p3"  # confianza en controles
    colnames(enc)[49] <- "p4"  # gestión satisface necesidades

    colnames(enc)[34] <- "abierta_1"
    colnames(enc)[35] <- "abierta_2"
    colnames(enc)[36] <- "abierta_3"
    colnames(enc)[37] <- "abierta_4"

    items <- c(paste0("f",1:4), paste0("s",1:4),
               paste0("i",1:4), paste0("r",1:4),
               paste0("p",1:4))

    # Convertir Likert a numérico fila por fila usando base R
    consent_ascii <- tolower(iconv(trimws(as.character(enc$acepta)),
                                    to="ASCII//TRANSLIT"))
    consent_ok <- consent_ascii %in% c(
      "si, acepto participar", "si acepto participar",
      "yes, i agree to participate", "yes", "accepted"
    )
    enc_base <- enc[consent_ok, , drop=FALSE]

    if ("industry" %in% tolower(names(enc_base))) {
      sector_col <- names(enc_base)[which(tolower(names(enc_base))=="industry")[1]]
      enc_base$sector <- as.character(enc_base[[sector_col]])
      vacio_sector <- is.na(enc_base$sector) | trimws(enc_base$sector)==""
      enc_base$sector[vacio_sector] <- empresa_a_sector(enc_base$empresa[vacio_sector])
    } else {
      enc_base$sector <- empresa_a_sector(enc_base$empresa)
    }
    if (!"respondent_type" %in% names(enc_base)) {
      enc_base$respondent_type <- "Not specified"
    }

    # Conversión Likert -> numérico columna por columna
    for (col_i in items) {
      enc_base[[col_i]] <- likert_num(enc_base[[col_i]])
    }

    # Filtrar filas con >= 12 ítems válidos
    n_validos <- rowSums(!is.na(enc_base[, items]))
    enc_base  <- enc_base[n_validos >= 12, ]

    # Calcular dimensiones con base R (evita conflictos de namespace)
    enc_base$FI <- rowMeans(enc_base[, c("f1","f2","f3","f4")], na.rm=TRUE)
    enc_base$SI <- rowMeans(enc_base[, c("s1","s2","s3","s4")], na.rm=TRUE)
    enc_base$IE <- rowMeans(enc_base[, c("i1","i2","i3","i4")], na.rm=TRUE)
    enc_base$RD <- rowMeans(enc_base[, c("r1","r2","r3","r4")], na.rm=TRUE)
    enc_base$PG <- rowMeans(enc_base[, c("p1","p2","p3","p4")], na.rm=TRUE)
    enc_base$EIglobal <- rowMeans(
      enc_base[, c("FI","SI","IE","RD","PG")], na.rm=TRUE)

    enc_limpia <- enc_base

    # Alpha de Cronbach con ítems disponibles
    items_ok <- items[colSums(!is.na(enc_limpia[,items])) >= 3]
    alpha_enc <- tryCatch(
      psych::alpha(enc_limpia[, items_ok]),
      error=function(e) NULL
    )

    # Scores globales (1 escalar por dimensión)
    scores_enc <- enc_limpia %>%
      summarise(
        n           = n(),
        FI          = mean(FI,  na.rm=TRUE),
        SI          = mean(SI,  na.rm=TRUE),
        IE          = mean(IE,  na.rm=TRUE),
        RD          = mean(RD,  na.rm=TRUE),
        PG          = mean(PG,  na.rm=TRUE),
        EIglobal    = mean(EIglobal, na.rm=TRUE)
      )

    scores_sector <- enc_limpia %>%
      dplyr::filter(!is.na(sector), trimws(as.character(sector)) != "") %>%
      dplyr::group_by(sector) %>%
      dplyr::summarise(
        n_respondentes=dplyr::n(),
        FI=mean(FI,na.rm=TRUE), SI=mean(SI,na.rm=TRUE),
        IE=mean(IE,na.rm=TRUE), RD=mean(RD,na.rm=TRUE),
        PG=mean(PG,na.rm=TRUE), EIglobal=mean(EIglobal,na.rm=TRUE),
        .groups="drop"
      ) %>%
      dplyr::rename(industria=sector)

    # Scores por sector permanecen exclusivamente en el módulo organizacional.
    # No se unen a `base`: el contraste técnico-organizacional se construye
    # explícitamente y nunca imputa sectores sin respondentes.
    tryCatch(write_csv(scores_sector, "outputs/tablas/08_scores_sector.csv"),
             error=function(e) NULL)

    # Textos abiertos
    texto_abierto <- enc_limpia %>%
      dplyr::select(starts_with("abierta_")) %>%
      pivot_longer(everything(),
                   names_to="pregunta", values_to="respuesta") %>%
      filter(!is.na(respuesta), nchar(trimws(respuesta)) > 5)

    tryCatch(write_csv(texto_abierto, "outputs/tablas/09_textos.csv"),
             error=function(e) NULL)
  }

  # Bandera estricta del módulo organizacional.
  # Si no hay cuestionario válido, FI/SI/IE/RD/PG/EIglobal NO existen
  # en el núcleo técnico y el módulo se reporta como Not available.
  tiene_encuesta <- !is.null(enc_limpia) && nrow(enc_limpia) > 0

  ## =========================================================
  ## FASE 4 — MINERÍA: PCA (prcomp) + K-MEANS + GLM
  ##   [v7.3.2] Espacio multivariado construido ÚNICAMENTE con
  ##   variables del dataset técnico de incidentes.
  ## =========================================================
  vars_cols <- c("n_incidentes","impacto_total","severidad_media",
                 "t_respuesta_media","duracion_media","tasa_exito",
                 "idx_resiliencia")

  vars_num_full <- base[, vars_cols]
  vars_num_full[] <- lapply(vars_num_full, as.numeric)
  filas_ok  <- complete.cases(vars_num_full)   # índice para remapear clusters
  vars_num  <- vars_num_full[filas_ok, , drop=FALSE]

  # Eliminar columnas de varianza cero antes de PCA
  vars_num_df <- as.data.frame(vars_num)
  keep <- sapply(vars_num_df, function(x) sd(x, na.rm=TRUE) > 1e-8)
  vars_num_df <- vars_num_df[, keep, drop=FALSE]

  # PCA con prcomp (base R, más robusto que FactoMineR)
  pca_res <- tryCatch({
    p <- prcomp(vars_num_df, center=TRUE, scale.=TRUE)
    # Adjuntar varianza para gráficos
    p$var_pct <- round(p$sdev^2 / sum(p$sdev^2) * 100, 1)
    p$var_cum <- cumsum(p$var_pct)
    p
  }, error=function(e) {
    message("PCA error: ", conditionMessage(e)); NULL
  })

  # Diagnóstico exportable de k y clustering final reproducible.
  k_diag <- diagnostico_k(vars_num_df, k_max=8L,
                          sample_n=400L, seed=RANDOM_SEED)

  k_selected_sil <- if (nrow(k_diag) && K_FINAL %in% k_diag$k)
    k_diag$silhouette_mean[k_diag$k == K_FINAL][1] else NA_real_

  k_sil_max <- if (nrow(k_diag) && any(is.finite(k_diag$silhouette_mean))) {
    kd_ok <- k_diag %>% dplyr::filter(is.finite(silhouette_mean))
    kd_ok$k[which.max(kd_ok$silhouette_mean)]
  } else NA_integer_

  k_sil_max_value <- if (nrow(k_diag) && is.finite(k_sil_max)) {
    k_diag$silhouette_mean[k_diag$k == k_sil_max][1]
  } else NA_real_

  tryCatch(
    readr::write_csv(k_diag, "outputs/tablas/10_diagnostico_k.csv"),
    error=function(e) NULL
  )

  set.seed(RANDOM_SEED)
  km <- tryCatch(
    kmeans(scale(vars_num_df), centers=K_FINAL, nstart=25),
    error=function(e) NULL
  )

  # [v7.3.3] Silhouette exacta de la partición final con TODAS las celdas
  # (el valor de diagnostico_k() se calcula en una submuestra de 400 y
  # solo se usa para elegir k).
  sil_final_full <- tryCatch({
    xs_f <- scale(vars_num_df)
    if (!is.null(km) && nrow(xs_f) <= 15000L)
      mean(cluster::silhouette(km$cluster, stats::dist(xs_f))[, 3])
    else NA_real_
  }, error=function(e) NA_real_)

  # [v7.3.2] Remapeo explícito de clusters a las filas originales de `base`
  # (evita asumir una correspondencia 1:1 que puede desalinearse).
  cl <- rep(NA_character_, nrow(base))
  if (!is.null(km) && length(km$cluster) == sum(filas_ok))
    cl[filas_ok] <- as.character(km$cluster)
  base$cluster <- factor(cl, levels=as.character(sort(unique(na.omit(as.numeric(cl))))))

  cluster_profiles <- caracterizar_clusters(base)
  tryCatch(
    readr::write_csv(cluster_profiles, "outputs/tablas/11_perfiles_clusters.csv"),
    error=function(e) NULL
  )

  # [v7.3.2] GLM con predictores exclusivamente técnicos.
  # Las dimensiones del cuestionario no entran al modelo.
  mdl_formula <- n_incidentes ~
    anio + industria + tipo_ataque +
    log_impacto + severidad_media + tasa_exito + idx_resiliencia

  mdl_pois <- tryCatch(
    glm(mdl_formula, family=poisson(link="log"), data=base),
    error=function(e) {
      message("GLM Poisson error: ", conditionMessage(e)); NULL
    }
  )

  # [v7.3.2] Diagnóstico formal de dispersión y elección del modelo.
  # Se usa X² de Pearson / gl residuales como estimador de phi y la
  # aproximación chi-cuadrado con DOS pruebas direccionales:
  #   - cola inferior  -> subdispersión significativa,
  #   - cola superior  -> sobredispersión significativa,
  #   - ninguna        -> dispersión compatible con Poisson.
  #
  # Sobredispersión  -> Binomial Negativa (si converge).
  # Subdispersión    -> Quasi-Poisson para corregir la inferencia;
  #                     AIC/BIC no están definidos bajo cuasi-verosimilitud.
  # Sin evidencia de dispersión -> Poisson.
  tipo_mdl <- "Poisson"
  mdl_final <- mdl_pois
  dispersion_diag <- list(
    dispersion_ratio = NA_real_,
    pearson_chisq     = NA_real_,
    df_residual       = NA_real_,
    p_under           = NA_real_,
    p_over            = NA_real_,
    status            = "Not available"
  )

  if (!is.null(mdl_pois)) {
    pearson_chisq <- tryCatch(
      sum(residuals(mdl_pois, type="pearson")^2, na.rm=TRUE),
      error=function(e) NA_real_
    )
    df_res <- tryCatch(df.residual(mdl_pois), error=function(e) NA_real_)
    ratio  <- if (is.finite(pearson_chisq) && is.finite(df_res) && df_res > 0)
      pearson_chisq / df_res else NA_real_

    p_under <- if (is.finite(pearson_chisq) && is.finite(df_res) && df_res > 0)
      stats::pchisq(pearson_chisq, df=df_res, lower.tail=TRUE) else NA_real_
    p_over <- if (is.finite(pearson_chisq) && is.finite(df_res) && df_res > 0)
      stats::pchisq(pearson_chisq, df=df_res, lower.tail=FALSE) else NA_real_

    status <- "Approximately equidispersed"
    if (is.finite(ratio) && ratio < 1 && is.finite(p_under) && p_under < 0.05)
      status <- "Underdispersed"
    if (is.finite(ratio) && ratio > 1 && is.finite(p_over) && p_over < 0.05)
      status <- "Overdispersed"

    dispersion_diag <- list(
      dispersion_ratio = ratio,
      pearson_chisq     = pearson_chisq,
      df_residual       = df_res,
      p_under           = p_under,
      p_over            = p_over,
      status            = status
    )

    if (identical(status, "Overdispersed")) {
      mdl_nb <- tryCatch(
        MASS::glm.nb(mdl_formula, data=base),
        error=function(e) NULL
      )
      if (!is.null(mdl_nb)) {
        mdl_final <- mdl_nb
        tipo_mdl  <- "Binomial Negativa"
      } else {
        tipo_mdl <- "Poisson (NB no convergió)"
      }
    } else if (identical(status, "Underdispersed")) {
      mdl_qp <- tryCatch(
        glm(mdl_formula, family=quasipoisson(link="log"), data=base),
        error=function(e) NULL
      )
      if (!is.null(mdl_qp)) {
        mdl_final <- mdl_qp
        tipo_mdl  <- "Quasi-Poisson"
      } else {
        tipo_mdl <- "Poisson (Quasi-Poisson no convergió)"
      }
    }
  }

  tbl_coef <- if (!is.null(mdl_final))
    tryCatch(broom::tidy(mdl_final, exponentiate=TRUE, conf.int=TRUE),
             error=function(e) tibble())
  else tibble()

  tryCatch(write_csv(tbl_coef, "outputs/tablas/05_coeficientes.csv"),
           error=function(e) NULL)

  ## =========================================================
  ## FASE 5 — INTERPRETACIÓN: valores ajustados + categoría
  ##   exploratoria de frecuencia (terciles), NO riesgo validado
  ## =========================================================
  base_res <- base %>%
    mutate(
      ajustados = if (!is.null(mdl_final))
        predict(mdl_final, type="response") else n_incidentes,
      error_pred = n_incidentes - ajustados,
      # [v7.3.2] Nombre de la columna alineado con lo que realmente
      # representa (tercil de la frecuencia ajustada).
      cat_frec_ajustada = {
        q <- unique(quantile(ajustados, c(0,0.33,0.66,1), na.rm=TRUE))
        if (length(q) >= 3) {
          cut(ajustados,
              breaks=quantile(ajustados, c(0,0.33,0.66,1), na.rm=TRUE),
              labels=c("Bajo","Medio","Alto"),
              include.lowest=TRUE) %>%
            factor(levels=c("Bajo","Medio","Alto"))
        } else {
          factor(ifelse(ajustados <= median(ajustados,na.rm=TRUE),"Bajo","Alto"),
                 levels=c("Bajo","Medio","Alto"))
        }
      }
    )

  # [v7.3.3] Análisis de sensibilidad: impacto medio, estabilidad de
  # terciles, PCA sin variables derivadas y estabilidad de la silueta.
  # (El COM-Poisson se ejecuta bajo demanda: pestaña "Sensibilidad".)
  sensibilidad <- tryCatch(
    analisis_sensibilidad(base, base_res, mdl_formula, vars_num_df,
                          km_cluster = if (!is.null(km)) km$cluster else NULL,
                          sil_sel = k_selected_sil, sil_full = sil_final_full),
    error=function(e) { message("Sensitivity error: ", conditionMessage(e)); NULL })

  # Alias de compatibilidad para valores ajustados.
  # [v7.3.2] `nivel_riesgo` se elimina definitivamente: los terciles
  # representan frecuencia ajustada exploratoria, no riesgo validado.
  base_res$predichos <- base_res$ajustados

  resumen_mdl <- tibble(
    framework_version= FRAMEWORK_VERSION,
    random_seed      = RANDOM_SEED,
    modelo           = tipo_mdl,
    aic              = tryCatch(AIC(mdl_final), error=function(e) NA_real_),
    bic              = tryCatch(BIC(mdl_final), error=function(e) NA_real_),
    dispersion       = dispersion_diag$dispersion_ratio,
    dispersion_status= dispersion_diag$status,
    k_selected       = K_FINAL,
    total_incidentes = sum(base$n_incidentes),
    impacto_total    = sum(base$impacto_total),
    severidad_global = mean(base$severidad_media, na.rm=TRUE),
    tasa_exito_global= mean(base$tasa_exito, na.rm=TRUE)
  )

  tryCatch({
    write_csv(base_res,    "outputs/tablas/06_predicciones.csv")
    write_csv(resumen_mdl, "outputs/tablas/07_resumen.csv")
  }, error=function(e) NULL)

  list(
    datos_orig     = datos_orig,
    datos_limpios  = datos_limpios,
    tbl_faltantes  = tbl_faltantes,
    resumen_limpie = resumen_limpieza,
    enc_limpia     = enc_limpia,
    alpha_enc      = alpha_enc,
    scores_enc     = scores_enc,
    scores_sector  = scores_sector,
    analysis_mode  = modo_analisis,
    tiene_encuesta = tiene_encuesta,
    texto_abierto  = texto_abierto,
    base           = base,
    vars_num       = vars_num,
    vars_cols      = vars_cols,
    pca_res        = pca_res,
    km             = km,
    k_diag         = k_diag,
    k_selected_sil = k_selected_sil,
    sil_final_full = sil_final_full,
    sensibilidad   = sensibilidad,
    k_sil_max      = k_sil_max,
    k_sil_max_value= k_sil_max_value,
    cluster_profiles= cluster_profiles,
    mdl_final      = mdl_final,
    mdl_formula    = mdl_formula,
    tipo_mdl       = tipo_mdl,
    dispersion_diag= dispersion_diag,
    k_selected     = K_FINAL,
    tbl_coef       = tbl_coef,
    base_res       = base_res,
    resumen_mdl    = resumen_mdl
  )
}

# ── 2B. GRÁFICOS DE PUBLICACIÓN (funciones puras) ────────────
# Cada función recibe la lista `r` devuelta por procesar_kdd()
# y retorna un objeto ggplot listo para exportar.

# 1. Dashboard KDD (4 paneles estilo publicación)
pub_plot_dashboard <- function(r) {
  dl <- r$datos_limpios
  br <- r$base_res

  # Panel A: Frecuencia por tipo de ataque
  d_tipo <- dl %>% dplyr::count(tipo_ataque, sort=TRUE) %>%
    dplyr::mutate(pct=round(n/sum(n)*100,1),
                  lbl=paste0(scales::comma(n),"\n(",pct,"%)"))
  pA <- ggplot2::ggplot(d_tipo, ggplot2::aes(reorder(tipo_ataque,n), n, fill=tipo_ataque)) +
    ggplot2::geom_col(show.legend=FALSE, width=0.75) +
    ggplot2::geom_text(ggplot2::aes(label=lbl), hjust=-0.05, size=3) +
    ggplot2::scale_fill_manual(values=pal_ataque) +
    ggplot2::coord_flip(clip="off") +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.32))) +
    ggplot2::labs(title="A. Distribución de ataques por tipo",
                  x=NULL, y="Frecuencia") + tema_pub()

  # Panel B: Impacto por industria
  d_ind <- dl %>% dplyr::group_by(industria) %>%
    dplyr::summarise(imp=sum(impacto,na.rm=TRUE)/1e6,
                     sev=mean(severidad,na.rm=TRUE), .groups="drop")
  pB <- ggplot2::ggplot(d_ind, ggplot2::aes(reorder(industria,imp), imp, fill=sev)) +
    ggplot2::geom_col(width=0.75) +
    ggplot2::geom_text(ggplot2::aes(label=round(imp,0)), hjust=-0.1, size=3) +
    ggplot2::scale_fill_gradient(low="#AED6F1", high="#F1948A",
                                 name="Sev.\nmedia") +
    ggplot2::coord_flip(clip="off") +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.28))) +
    ggplot2::labs(title="B. Impacto económico por industria (M)",
                  x=NULL, y="Impacto (M, escala proporcional)") + tema_pub()

  # Panel C: Tasa éxito y tiempo de respuesta por ataque
  d_def <- dl %>% dplyr::group_by(tipo_ataque) %>%
    dplyr::summarise(tasa=mean(exito,na.rm=TRUE)*100,
                     resp=mean(t_respuesta_min,na.rm=TRUE), .groups="drop")
  pC <- ggplot2::ggplot(d_def, ggplot2::aes(resp, tasa, color=tipo_ataque,
                                              label=tipo_ataque)) +
    ggplot2::geom_point(size=4, alpha=0.9) +
    ggrepel::geom_text_repel(size=3.2, max.overlaps=20, show.legend=FALSE) +
    ggplot2::scale_color_manual(values=pal_ataque, name="Tipo ataque") +
    ggplot2::geom_hline(yintercept=mean(d_def$tasa), linetype="dashed", color="gray60") +
    ggplot2::geom_vline(xintercept=mean(d_def$resp), linetype="dashed", color="gray60") +
    ggplot2::labs(title="C. Tasa éxito vs. tiempo de respuesta",
                  x="Tiempo de respuesta medio (min)",
                  y="Tasa de éxito del ataque (%)") + tema_pub()

  # Panel D: terciles de la frecuencia ajustada
  d_risk <- br %>% dplyr::count(cat_frec_ajustada) %>%
    dplyr::filter(!is.na(cat_frec_ajustada)) %>%
    dplyr::mutate(pct=round(n/sum(n)*100,1),
                  lbl=paste0(pct,"%"))
  pD <- ggplot2::ggplot(d_risk, ggplot2::aes(cat_frec_ajustada, n, fill=cat_frec_ajustada)) +
    ggplot2::geom_col(width=0.6, show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(label=lbl), vjust=-0.4, size=4, fontface="bold",
                       color="#4A4A4A") +
    ggplot2::scale_fill_manual(values=pal_riesgo) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.2))) +
    ggplot2::labs(title="D. Categoría exploratoria de frecuencia ajustada (terciles)",
                  x="Tercil de frecuencia ajustada", y="N.° de grupos") + tema_pub()

  (pA | pB) / (pC | pD) +
    patchwork::plot_annotation(
      title="Dashboard resumen: Proceso KDD y resultados clave",
      subtitle="Distribución de ataques · Impacto por industria · Eficacia defensiva · Frecuencia ajustada (terciles)",
      caption=paste0("Fuente: datos técnicos de incidentes | GLM Poisson / Quasi-Poisson / Binomial Negativa según diagnóstico de dispersión. ",
                     CAT_FREC_TXT),
      theme=ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=16,color="#34495E"),
                           plot.subtitle=ggplot2::element_text(color="gray45",size=12)))
}

# 2. Diagnóstico de k (Elbow + Silhouette)
#    [v7.3.2] El gráfico documenta el valor usado (K_FINAL) y el máximo
#    empírico de Silhouette por separado: no se afirma que coincidan.
pub_plot_elbow <- function(r) {
  d <- r$k_diag
  if (is.null(d) || nrow(d) == 0)
    stop("Datos insuficientes para el diagnóstico de k")

  dw <- d %>% dplyr::select(k, wss=WSS)
  ds <- d %>%
    dplyr::filter(k >= 2, is.finite(silhouette_mean)) %>%
    dplyr::transmute(k, sil=silhouette_mean)

  k_sil_max <- r$k_sil_max

  pW <- ggplot2::ggplot(dw, ggplot2::aes(k, wss)) +
    ggplot2::geom_line(color="#7FB3D5", linewidth=1.3) +
    ggplot2::geom_point(size=3.5, color="#7FB3D5") +
    ggplot2::geom_point(data=dw[dw$k == r$k_selected,,drop=FALSE],
                        size=5, shape=8, color="#F1948A", stroke=1.5) +
    ggplot2::scale_x_continuous(breaks=dw$k) +
    ggplot2::labs(title="A. Método del codo (Within-cluster SS)",
                  subtitle=paste0("★ = solución final retenida (k = ",
                                  r$k_selected, ")"),
                  x="Número de clusters (k)", y="WSS total") + tema_pub()

  pS <- ggplot2::ggplot(ds, ggplot2::aes(k, sil)) +
    ggplot2::geom_line(color="#7DCEA0", linewidth=1.3) +
    ggplot2::geom_point(size=3.5, color="#7DCEA0") +
    ggplot2::geom_point(
      data=ds[ds$k == k_sil_max,,drop=FALSE],
      size=5, shape=8, color="#F1948A", stroke=1.5) +
    ggplot2::geom_vline(xintercept=r$k_selected, linetype="dashed",
                        color="#7FB3D5", linewidth=0.9) +
    ggplot2::scale_x_continuous(breaks=ds$k) +
    ggplot2::labs(
      title="B. Coeficiente Silhouette medio",
      subtitle=paste0(
        "Silhouette k=", r$k_selected, ": ",
        round(r$k_selected_sil,3),
        " | máximo empírico: k=", k_sil_max,
        " (", round(r$k_sil_max_value,3), ")"),
      x="Número de clusters (k)", y="Silhouette medio") + tema_pub()

  (pW | pS) +
    patchwork::plot_annotation(
      title="Diagnóstico del número de clusters K-Means",
      caption=paste0(
        K_FINAL_RATIONALE, " ",
        if (is.finite(k_sil_max) && k_sil_max != r$k_selected)
          paste0("El máximo empírico de Silhouette se observa en k = ",
                 k_sil_max, "; esta discrepancia se reporta de forma transparente. ")
        else
          "La solución retenida coincide con el máximo empírico de Silhouette. ",
        "Diagnóstico calculado exclusivamente sobre variables técnicas; ",
        "Silhouette con muestra reproducible de hasta n=400 y semilla ",
        RANDOM_SEED, "."),
      theme=ggplot2::theme(
        plot.title=ggplot2::element_text(face="bold",size=15,color="#34495E")))
}

# 3. Heatmap de centroides de clusters
pub_plot_heatmap <- function(r) {
  km <- r$km
  if (is.null(km)) stop("K-Means no disponible")
  ctrs <- as.data.frame(scale(km$centers))
  ctrs$Cluster <- paste0("Cluster ", rownames(ctrs))
  d_long <- ctrs %>%
    tidyr::pivot_longer(-Cluster, names_to="Variable", values_to="z")
  # Solo etiquetas de variables técnicas
  labs_var <- c(
    n_incidentes="Frecuencia\nincidentes", impacto_total="Impacto\ntotal",
    severidad_media="Severidad\nmedia", t_respuesta_media="T. respuesta",
    duracion_media="Duración\nataque", tasa_exito="Tasa éxito\nataque",
    idx_resiliencia="Resiliencia"
  )
  d_long$Variable <- dplyr::recode(d_long$Variable, !!!labs_var)
  ggplot2::ggplot(d_long, ggplot2::aes(Variable, Cluster, fill=z)) +
    ggplot2::geom_tile(color="white", linewidth=0.5) +
    ggplot2::geom_text(ggplot2::aes(label=round(z,2)),
                       size=3.5, fontface="bold",
                       color=ifelse(abs(d_long$z)>1,"white","gray30")) +
    ggplot2::scale_fill_gradient2(low="#7FB3D5", mid="white", high="#F1948A",
                                  midpoint=0, name="Z-score\n(σ)",
                                  limits=c(-2.5,2.5), oob=scales::squish) +
    ggplot2::labs(
      title="Heatmap de centroides de clusters (valores estandarizados)",
      subtitle="Rosado = valor alto | Azul = valor bajo | Blanco = media global",
      x=NULL, y=NULL,
      caption=paste0("Z-score sobre los centroides de K-Means (k = ", r$k_selected,
                     "), calculado únicamente con variables técnicas")) +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30,hjust=1,size=10),
                   plot.title=ggplot2::element_text(face="bold",color="#34495E"),
                   plot.subtitle=ggplot2::element_text(color="gray45"),
                   plot.caption=ggplot2::element_text(color="gray55",hjust=0))
}

# 4. Boxplot de variables clave por cluster
pub_plot_boxclust <- function(r) {
  bm <- r$base
  if (!"cluster" %in% names(bm) || dplyr::n_distinct(bm$cluster, na.rm=TRUE) < 2)
    stop("Cluster no disponible")

  vars_box <- c("n_incidentes","severidad_media","tasa_exito","idx_resiliencia")
  labs_box <- c("Frecuencia\nincidentes","Severidad\nmedia",
                "Tasa éxito\nataque","Índice\nresiliencia")

  # Contraste externo únicamente cuando existe cuestionario.
  if (isTRUE(r$tiene_encuesta) && !is.null(r$scores_sector)) {
    bm <- bm %>%
      dplyr::left_join(
        r$scores_sector %>% dplyr::select(industria, EIglobal),
        by="industria"
      )
    vars_box <- c(vars_box, "EIglobal")
    labs_box <- c(labs_box, "EIglobal\n(externo)")
  }

  d_long <- bm %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::select(cluster, dplyr::all_of(vars_box)) %>%
    tidyr::pivot_longer(-cluster, names_to="Variable", values_to="Valor") %>%
    dplyr::mutate(Variable=factor(Variable, levels=vars_box, labels=labs_box))

  ggplot2::ggplot(d_long, ggplot2::aes(cluster, Valor, fill=cluster)) +
    ggplot2::geom_boxplot(alpha=0.85, outlier.alpha=0.3, outlier.size=1) +
    ggplot2::geom_jitter(width=0.15, alpha=0.12, size=0.8, color="#5D6D7E") +
    ggplot2::scale_fill_manual(values=pal_cluster, name="Cluster") +
    ggplot2::facet_wrap(~Variable, scales="free_y", nrow=1) +
    ggplot2::labs(
      title=paste0("Distribución de variables por cluster técnico (K-Means, k = ",
                   r$k_selected, ")"),
      subtitle=if (isTRUE(r$tiene_encuesta))
        "EIglobal se muestra solo como external contrast; no participó en el clustering"
      else
        "Optional Organizational Module: Not available",
      x="Cluster", y="Valor",
      caption="Clusters construidos exclusivamente con datos técnicos de incidentes") +
    tema_pub() + ggplot2::theme(legend.position="none")
}

# 5. Matriz de correlación
pub_plot_corrmat <- function(r) {
  vn <- as.data.frame(r$vars_num)
  # Solo etiquetas de variables técnicas
  labs_corr <- c(
    n_incidentes="Freq.\nincidentes", impacto_total="Impacto\ntotal",
    severidad_media="Severidad", t_respuesta_media="T.resp.",
    duracion_media="Duración", tasa_exito="Tasa\néxito",
    idx_resiliencia="Resiliencia"
  )
  colnames(vn) <- dplyr::recode(colnames(vn), !!!labs_corr)
  cm <- cor(vn, use="complete.obs", method="pearson")
  d_cor <- as.data.frame(as.table(cm)) %>%
    dplyr::rename(x=Var1, y=Var2, r=Freq) %>%
    dplyr::mutate(r=round(r,2), label=ifelse(abs(r)>=0.4, as.character(r),""))
  ggplot2::ggplot(d_cor, ggplot2::aes(x, y, fill=r)) +
    ggplot2::geom_tile(color="white", linewidth=0.5) +
    ggplot2::geom_text(ggplot2::aes(label=label), size=3,
                       color=ifelse(abs(d_cor$r)>0.7,"white","gray30")) +
    ggplot2::scale_fill_gradient2(low="#7FB3D5", mid="white", high="#F1948A",
                                  midpoint=0, limits=c(-1,1), name="Pearson\nr") +
    ggplot2::labs(
      title="Matriz de correlación de Pearson — Variables técnicas del modelo",
      subtitle="Solo se muestran etiquetas cuando |r| ≥ 0.40",
      x=NULL, y=NULL,
      caption="Azul = correlación negativa | Rosado = correlación positiva | Blanco = sin correlación lineal") +
    ggplot2::theme_minimal(base_size=12) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1),
                   plot.title=ggplot2::element_text(face="bold",color="#34495E"),
                   plot.subtitle=ggplot2::element_text(color="gray45"),
                   plot.caption=ggplot2::element_text(color="gray55",hjust=0))
}

# 6. Boxplot de variables por industria y tipo de ataque
pub_plot_boxplot <- function(r) {
  dl <- r$datos_limpios
  pA <- ggplot2::ggplot(dl, ggplot2::aes(reorder(tipo_ataque,severidad,median),
                                          severidad, fill=tipo_ataque)) +
    ggplot2::geom_boxplot(alpha=0.85, outlier.alpha=0.2, show.legend=FALSE) +
    ggplot2::scale_fill_manual(values=pal_ataque) +
    ggplot2::coord_flip() +
    ggplot2::labs(title="A. Severidad por tipo de ataque",
                  x=NULL, y="Severidad (1-10)") + tema_pub()

  pB <- ggplot2::ggplot(dl, ggplot2::aes(reorder(industria,t_respuesta_min,median),
                                          t_respuesta_min, fill=industria)) +
    ggplot2::geom_boxplot(alpha=0.85, outlier.alpha=0.2, show.legend=FALSE) +
    ggplot2::scale_fill_brewer(palette="Pastel1") +
    ggplot2::coord_flip() +
    ggplot2::labs(title="B. Tiempo de respuesta por industria (min)",
                  x=NULL, y="Tiempo de respuesta (min)") + tema_pub()

  pC <- ggplot2::ggplot(dl, ggplot2::aes(reorder(mitigacion,gb_comprometidos,median),
                                          gb_comprometidos, fill=mitigacion)) +
    ggplot2::geom_boxplot(alpha=0.85, outlier.alpha=0.2, show.legend=FALSE) +
    ggplot2::scale_fill_brewer(palette="Pastel2") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10(labels=scales::comma) +
    ggplot2::labs(title="C. GB comprometidos por método de mitigación (log)",
                  subtitle="Muestra el volumen de datos comprometidos según el método usado",
                  x=NULL, y="GB comprometidos (escala log)") + tema_pub()

  pD <- ggplot2::ggplot(dl, ggplot2::aes(reorder(sistema,duracion_min,median),
                                          duracion_min, fill=sistema)) +
    ggplot2::geom_boxplot(alpha=0.85, outlier.alpha=0.2, show.legend=FALSE) +
    ggplot2::scale_fill_brewer(palette="Set3") +
    ggplot2::coord_flip() +
    ggplot2::labs(title="D. Duración del ataque por sistema objetivo (min)",
                  x=NULL, y="Duración (min)") + tema_pub()

  (pA | pB) / (pC | pD) +
    patchwork::plot_annotation(
      title="Distribución de variables clave por categoría (Boxplot)",
      caption="Línea central = mediana | Caja = IQR | Bigotes = 1.5×IQR | Puntos = valores atípicos",
      theme=ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=15,color="#34495E")))
}

# 7. Diagramas de densidad
pub_plot_density <- function(r) {
  dl <- r$datos_limpios
  pA <- ggplot2::ggplot(dl, ggplot2::aes(severidad, fill=tipo_ataque, color=tipo_ataque)) +
    ggplot2::geom_density(alpha=0.30, linewidth=0.8) +
    ggplot2::scale_fill_manual(values=pal_ataque, name="Tipo")  +
    ggplot2::scale_color_manual(values=pal_ataque, name="Tipo") +
    ggplot2::labs(title="A. Densidad de severidad por tipo de ataque",
                  x="Severidad (1-10)", y="Densidad") + tema_pub()

  pB <- ggplot2::ggplot(dl, ggplot2::aes(log1p(impacto), fill=industria, color=industria)) +
    ggplot2::geom_density(alpha=0.25, linewidth=0.8) +
    ggplot2::scale_fill_brewer(palette="Pastel1", name="Industria") +
    ggplot2::scale_color_brewer(palette="Pastel1", name="Industria") +
    ggplot2::labs(title="B. Densidad del impacto económico por industria (log)",
                  x="log(1+Impacto)", y="Densidad") + tema_pub()

  pC <- ggplot2::ggplot(dl, ggplot2::aes(t_respuesta_min, fill=resultado, color=resultado)) +
    ggplot2::geom_density(alpha=0.45, linewidth=1) +
    ggplot2::scale_fill_manual(values=c(Success="#A9DFBF",Failure="#F5B7B1"),name="Resultado") +
    ggplot2::scale_color_manual(values=c(Success="#7DCEA0",Failure="#F1948A"),name="Resultado") +
    ggplot2::labs(title="C. Tiempo de respuesta según resultado del ataque",
                  x="Tiempo de respuesta (min)", y="Densidad") + tema_pub()

  pD <- ggplot2::ggplot(dl, ggplot2::aes(duracion_min, fill=herramienta, color=herramienta)) +
    ggplot2::geom_density(alpha=0.25, linewidth=0.8) +
    ggplot2::scale_fill_brewer(palette="Pastel2", name="Herramienta") +
    ggplot2::scale_color_brewer(palette="Pastel2", name="Herramienta") +
    ggplot2::labs(title="D. Duración del ataque por herramienta de seguridad",
                  x="Duración (min)", y="Densidad") + tema_pub()

  (pA | pB) / (pC | pD) +
    patchwork::plot_annotation(
      title="Diagramas de densidad — Variables clave del dataset",
      caption="Cada curva representa la distribución de probabilidad estimada por núcleo (KDE)",
      theme=ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=15,color="#34495E")))
}

# 8. Forest Plot de coeficientes del modelo
pub_plot_forest <- function(r) {
  if (is.null(r$mdl_final) || nrow(r$tbl_coef)==0) stop("Modelo no disponible")
  d <- r$tbl_coef %>%
    dplyr::filter(term != "(Intercept)", !is.na(estimate)) %>%
    dplyr::mutate(
      sig   = dplyr::case_when(p.value<.001~"p<0.001",p.value<.01~"p<0.01",
                                p.value<.05~"p<0.05",TRUE~"n.s."),
      sig   = factor(sig,levels=c("p<0.001","p<0.01","p<0.05","n.s.")),
      term  = stringr::str_replace_all(term, c("industria"="Ind: ","tipo_ataque"="Ataque: ")),
      efecto= ifelse(estimate>1,"Aumenta incidentes","Reduce incidentes")
    ) %>%
    dplyr::arrange(dplyr::desc(estimate))

  ggplot2::ggplot(d, ggplot2::aes(estimate, reorder(term, estimate), color=sig)) +
    ggplot2::geom_vline(xintercept=1, linetype="dashed", color="gray55", linewidth=0.8) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=conf.low, xmax=conf.high),
                            height=0.3, linewidth=0.8, alpha=0.8) +
    ggplot2::geom_point(size=3.5) +
    ggplot2::scale_color_manual(
      values=c("p<0.001"="#F1948A","p<0.01"="#F0B27A","p<0.05"="#F9E79F","n.s."="#BFC9CA"),
      name="Significancia") +
    ggplot2::scale_x_log10(labels=scales::comma) +
    ggplot2::labs(
      title="Forest Plot — Coeficientes del modelo GLM (IRR con IC 95%)",
      subtitle="IRR > 1 = aumenta frecuencia de incidentes | IRR < 1 = efecto protector | Escala logarítmica",
      x="Incidence Rate Ratio (IRR) — escala log",
      y=NULL,
      caption=paste0("Modelo ajustado solo con predictores técnicos. ",
                     "IC = Intervalo de Confianza al 95% | Colores por nivel de significancia estadística")) +
    tema_pub()
}

# 9. Observado vs ajustado con residuales
pub_plot_predreal <- function(r) {
  d <- r$base_res
  # r² = cor(observado, ajustado)^2, no el R² de un modelo lineal
  r2  <- tryCatch(cor(d$n_incidentes, d$ajustados, use="complete.obs")^2,
                  error=function(e) NA)
  mae <- mean(abs(d$error_pred),na.rm=TRUE)

  pA <- ggplot2::ggplot(d, ggplot2::aes(n_incidentes, ajustados, color=cat_frec_ajustada)) +
    ggplot2::geom_point(alpha=0.55, size=1.8) +
    ggplot2::geom_abline(slope=1, intercept=0, color="#F1948A",
                         linetype="dashed", linewidth=1) +
    ggplot2::geom_smooth(method="lm", se=TRUE, color="#5D6D7E",
                         fill="#D5DBDB", alpha=0.3, show.legend=FALSE) +
    ggplot2::scale_color_manual(values=pal_riesgo, name="Tercil frec.\najustada") +
    ggplot2::labs(title=paste0("A. Observados vs. ajustados (r² Pearson = ",round(r2,3),")"),
                  subtitle="Línea rosada = ajuste perfecto | Gris = regresión lineal observada",
                  x="Frecuencia observada de incidentes",
                  y="Frecuencia ajustada") + tema_pub()

  pB <- ggplot2::ggplot(d, ggplot2::aes(ajustados, error_pred)) +
    ggplot2::geom_hline(yintercept=0, color="#F1948A", linewidth=0.9, linetype="dashed") +
    ggplot2::geom_point(alpha=0.45, color="#7FB3D5", size=1.8) +
    ggplot2::geom_smooth(se=TRUE, color="#5D6D7E", fill="#D5DBDB",
                         alpha=0.3, method="loess") +
    ggplot2::labs(title=paste0("B. Residuos vs. valores ajustados (MAE=",round(mae,2),")"),
                  subtitle="Dispersión aleatoria alrededor de 0 = buen ajuste del modelo",
                  x="Ajustados", y="Residuo (Observado − Ajustado)") + tema_pub()

  (pA | pB) +
    patchwork::plot_annotation(
      title="Diagnóstico del modelo GLM — Ajuste y residuos",
      caption=paste0("r² = correlación de Pearson al cuadrado entre valores observados y ajustados ",
                     "(no es el R² de un modelo lineal ni una medida de predicción fuera de muestra) | ",
                     "r² = ",round(r2,4)," | MAE = ",round(mae,2),
                     " | Modelo: ",r$tipo_mdl),
      theme=ggplot2::theme(plot.title=ggplot2::element_text(face="bold",size=15,color="#34495E")))
}

# 10. Tabla resumen para publicación
pub_tabla_resumen <- function(r) {
  r$datos_limpios %>%
    dplyr::group_by(industria, tipo_ataque) %>%
    dplyr::summarise(
      n                = dplyr::n(),
      pct_exito        = round(mean(exito,na.rm=TRUE)*100, 1),
      severidad_media  = round(mean(severidad,na.rm=TRUE), 2),
      t_resp_media     = round(mean(t_respuesta_min,na.rm=TRUE), 1),
      duracion_media   = round(mean(duracion_min,na.rm=TRUE), 1),
      gb_media         = round(mean(gb_comprometidos,na.rm=TRUE), 2),
      .groups="drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n))
}

# 11. Optional Organizational Module: dimensiones por sector
pub_plot_organizacional <- function(r) {
  if (is.null(r$scores_sector) || nrow(r$scores_sector) == 0)
    stop("Organizational questionnaire not provided — optional module")
  d <- r$scores_sector %>%
    tidyr::pivot_longer(c(FI,SI,IE,RD,PG,EIglobal),
                        names_to="Dimensión", values_to="Media") %>%
    dplyr::mutate(Dimensión=factor(Dimensión,
      levels=c("FI","SI","IE","RD","PG","EIglobal"),
      labels=c("FI","SI","IE","RD","PG","EIglobal")))

  ggplot2::ggplot(d, ggplot2::aes(Dimensión, Media, fill=Dimensión)) +
    ggplot2::geom_col(width=0.7, show.legend=FALSE) +
    ggplot2::geom_hline(yintercept=3, linetype="dashed", color="gray55") +
    ggplot2::geom_text(ggplot2::aes(label=round(Media,2)),
                       vjust=-0.4, size=3, color="#4A4A4A") +
    ggplot2::facet_wrap(~industria) +
    ggplot2::scale_y_continuous(limits=c(0,5.4), breaks=1:5) +
    ggplot2::scale_fill_manual(values=c("#85C1E9","#F5B7B1","#A9DFBF",
                                        "#FAD7A0","#C39BD3","#76D7C4")) +
    ggplot2::labs(
      title="Optional Organizational Module — Dimensiones de efectividad por sector",
      subtitle="Bloque independiente del modelo estadístico: no participa en PCA, K-Means ni GLM",
      x=NULL, y="Media Likert (1–5)",
      caption="FI=Frecuencia · SI=Severidad · IE=Impacto económico · RD=Respuesta/Recuperación · PG=Percepción global · EIglobal=promedio de las cinco") +
    tema_pub()
}

# ── 2C. EXPORTADOR DE PUBLICACIÓN ────────────────────────────
# Genera TODOS los gráficos en PDF + JPG + HTML y los guarda
# en outputs/article_figures/{pdf,jpg,html}. Devuelve el registro.

# ── 2C. ARTICLE FIGURES — export-only, never rendered in Shiny ─────────────
article_theme <- function() {
  ggplot2::theme_bw(base_size=12) +
    ggplot2::theme(
      plot.title=ggplot2::element_text(face="bold", size=13),
      plot.subtitle=ggplot2::element_text(size=10, color="gray35"),
      plot.caption=ggplot2::element_text(size=8.5, color="gray45", hjust=0),
      panel.grid.minor=ggplot2::element_blank(),
      legend.position="right"
    )
}

article_save <- function(p, stem, width=8.5, height=5.5, interactive=FALSE) {
  dirs <- c(pdf="outputs/article_figures/pdf",
            jpg="outputs/article_figures/jpg",
            html="outputs/article_figures/html")
  invisible(lapply(dirs, dir_crear))
  pdf_file <- file.path(dirs["pdf"], paste0(stem, ".pdf"))
  jpg_file <- file.path(dirs["jpg"], paste0(stem, ".jpg"))
  html_file <- file.path(dirs["html"], paste0(stem, ".html"))
  ok_pdf <- tryCatch({ggplot2::ggsave(pdf_file,p,width=width,height=height,device="pdf"); TRUE},
                     error=function(e) FALSE)
  ok_jpg <- tryCatch({ggplot2::ggsave(jpg_file,p,width=width,height=height,dpi=600,device="jpeg"); TRUE},
                     error=function(e) FALSE)
  ok_html <- FALSE
  if (isTRUE(interactive)) {
    ok_html <- tryCatch({
      htmlwidgets::saveWidget(plotly::ggplotly(p), html_file, selfcontained=TRUE)
      TRUE
    }, error=function(e) FALSE)
  }
  tibble::tibble(
    Figure=stem,
    PDF=if (ok_pdf) pdf_file else "Error",
    JPG=if (ok_jpg) jpg_file else "Error",
    HTML=if (interactive) if (ok_html) html_file else "Error" else "Not applicable",
    Status=if (ok_pdf && ok_jpg && (!interactive || ok_html)) "Exported" else "Partial"
  )
}

generate_article_figures <- function(r) {
  stopifnot(is.list(r))
  registry <- list()
  add <- function(x) registry[[length(registry)+1L]] <<- x
  tech <- r$base %>% dplyr::select(dplyr::all_of(intersect(r$vars_cols, names(r$base))))
  tech[] <- lapply(tech, as.numeric)

  # 1 Correlation heatmap
  if (ncol(tech) >= 2) {
    cm <- stats::cor(tech, use="pairwise.complete.obs")
    cd <- as.data.frame(as.table(cm))
    names(cd) <- c("Variable1","Variable2","Correlation")
    cd$Variable1 <- factor(cd$Variable1, levels=colnames(cm),
                           labels=unname(TECH_LABELS[colnames(cm)]))
    cd$Variable2 <- factor(cd$Variable2, levels=colnames(cm),
                           labels=unname(TECH_LABELS[colnames(cm)]))
    p <- ggplot2::ggplot(cd, ggplot2::aes(Variable1,Variable2,fill=Correlation)) +
      ggplot2::geom_tile(color="white",linewidth=.3) +
      ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",Correlation)),size=3) +
      ggplot2::scale_fill_gradient2(low="#85C1E9",mid="white",high="#F1948A",
                                    midpoint=0,limits=c(-1,1)) +
      ggplot2::labs(title="Correlation Heatmap",x=NULL,y=NULL,fill="Pearson r",
                    caption="Technical variables only.") +
      article_theme() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=40,hjust=1))
    add(article_save(p,"Figure_Correlation_Heatmap",8.5,7))
  }

  # 2 Time series
  dl <- r$datos_limpios
  if (!is.null(dl) && "fecha" %in% names(dl) && any(!is.na(dl$fecha))) {
    d <- dl %>% dplyr::filter(!is.na(fecha)) %>%
      dplyr::mutate(period=lubridate::floor_date(fecha,"month")) %>%
      dplyr::count(period,name="Incidents")
    p <- ggplot2::ggplot(d,ggplot2::aes(period,Incidents)) +
      ggplot2::geom_line(linewidth=.8,color="#5D8AA8") +
      ggplot2::geom_point(size=2,color="#F1948A") +
      ggplot2::labs(title="Cybersecurity Incidents Over Time",
                    subtitle="Monthly observed incident counts",
                    x="Month",y="Incident count",caption="Technical incident records only.") +
      article_theme()
    add(article_save(p,"Figure_Time_Series",9,5.5,interactive=TRUE))
  }

  pca <- r$pca_res
  if (!is.null(pca)) {
    vp <- pca$sdev^2/sum(pca$sdev^2)*100
    d <- tibble::tibble(PC=factor(paste0("PC",seq_along(vp)),levels=paste0("PC",seq_along(vp))),
                        Variance=vp,Cumulative=cumsum(vp))
    p <- ggplot2::ggplot(d,ggplot2::aes(PC,Variance)) +
      ggplot2::geom_col(fill="#AED6F1",width=.72) +
      ggplot2::geom_line(ggplot2::aes(y=Cumulative,group=1),color="#D77A7A",linewidth=.9) +
      ggplot2::geom_point(ggplot2::aes(y=Cumulative),color="#D77A7A",size=2.4) +
      ggplot2::labs(title="PCA Scree Plot",subtitle="Individual and cumulative explained variance",
                    x="Principal component",y="Explained variance (%)",
                    caption="PCA uses technical variables only.") + article_theme()
    add(article_save(p,"Figure_PCA_Scree",8.5,5.5))

    if (ncol(pca$x)>=2) {
      sc <- as.data.frame(pca$x[,1:2,drop=FALSE]); names(sc)<-c("PC1","PC2")
      sc$Profile <- if (!is.null(r$km) && length(r$km$cluster)==nrow(sc))
        factor(r$km$cluster) else factor("1")
      rot <- as.data.frame(pca$rotation[,1:2,drop=FALSE]); names(rot)<-c("PC1","PC2")
      sf <- min(max(abs(sc$PC1),na.rm=TRUE)/max(abs(rot$PC1),na.rm=TRUE),
                max(abs(sc$PC2),na.rm=TRUE)/max(abs(rot$PC2),na.rm=TRUE))*.65
      rot <- rot*sf; rot$Variable <- unname(TECH_LABELS[rownames(rot)])
      p <- ggplot2::ggplot(sc,ggplot2::aes(PC1,PC2,color=Profile)) +
        ggplot2::geom_point(alpha=.55,size=1.8) +
        ggplot2::geom_segment(data=rot,ggplot2::aes(x=0,y=0,xend=PC1,yend=PC2),
          inherit.aes=FALSE,arrow=ggplot2::arrow(length=grid::unit(.18,"cm")),color="gray35") +
        ggrepel::geom_text_repel(data=rot,ggplot2::aes(PC1,PC2,label=Variable),
          inherit.aes=FALSE,size=3.1,color="gray20") +
        ggplot2::scale_color_manual(values=pal_cluster,name="Exploratory technical profile") +
        ggplot2::labs(title="PCA Biplot — Technical Variables",
          x=sprintf("PC1 (%.2f%%)",vp[1]),y=sprintf("PC2 (%.2f%%)",vp[2]),
          caption="Clusters are exploratory technical profiles; separation may be limited.") +
        article_theme()
      add(article_save(p,"Figure_PCA_Biplot",9,6.5,interactive=TRUE))
    }
  }

  # 5 Elbow
  kd <- r$k_diag
  if (!is.null(kd) && nrow(kd)) {
    p <- ggplot2::ggplot(kd,ggplot2::aes(k,WSS)) +
      ggplot2::geom_line(linewidth=.8,color="#5D8AA8") + ggplot2::geom_point(size=2.6,color="#F1948A") +
      ggplot2::geom_vline(xintercept=r$k_selected,linetype="dashed") +
      ggplot2::scale_x_continuous(breaks=kd$k) +
      ggplot2::labs(title="K-Means Elbow / WSS Plot",
        subtitle=paste0("Final retained solution: k = ",r$k_selected),
        x="Number of clusters (k)",y="Within-cluster sum of squares",
        caption=K_FINAL_RATIONALE) + article_theme()
    add(article_save(p,"Figure_KMeans_Elbow",8.5,5.5))
  }

  # 6 Silhouette plot for retained k
  if (!is.null(r$km) && nrow(r$vars_num) > r$k_selected) {
    xs0 <- as.data.frame(r$vars_num)
    xs0 <- xs0[,vapply(xs0,function(x) stats::sd(x,na.rm=TRUE)>1e-8,logical(1)),drop=FALSE]
    xs <- scale(xs0)
    sil <- tryCatch(cluster::silhouette(r$km$cluster,stats::dist(xs)),error=function(e) NULL)
    if (!is.null(sil)) {
      sd <- as.data.frame(sil); sd$Observation <- seq_len(nrow(sd))
      sd$Profile <- factor(sd$cluster)
      sd <- sd[order(sd$cluster,sd$sil_width),]; sd$Order <- seq_len(nrow(sd))
      p <- ggplot2::ggplot(sd,ggplot2::aes(Order,sil_width,fill=Profile)) +
        ggplot2::geom_col(width=1) + ggplot2::geom_hline(yintercept=mean(sd$sil_width),
          linetype="dashed",linewidth=.7) +
        ggplot2::scale_fill_manual(values=pal_cluster,name="Exploratory technical profile") +
        ggplot2::labs(title="K-Means Silhouette Plot",
          subtitle=sprintf("Mean silhouette = %.3f; limited separation should be acknowledged",mean(sd$sil_width)),
          x="Observations ordered within profile",y="Silhouette width",
          caption="k = 3 retained using elbow, silhouette and interpretability jointly.") +
        article_theme()
      add(article_save(p,"Figure_KMeans_Silhouette",9,5.5))
    }
  }

  # 7 standardized centroids
  if (!is.null(r$km)) {
    cen <- as.data.frame(r$km$centers); cen$Profile <- factor(seq_len(nrow(cen)))
    cd <- tidyr::pivot_longer(cen,-Profile,names_to="Variable",values_to="Z")
    cd$Variable <- factor(cd$Variable,levels=names(TECH_LABELS),
                          labels=unname(TECH_LABELS[names(TECH_LABELS)]))
    p <- ggplot2::ggplot(cd,ggplot2::aes(Variable,Profile,fill=Z)) +
      ggplot2::geom_tile(color="white") +
      ggplot2::scale_fill_gradient2(low="#85C1E9",mid="white",high="#F1948A",midpoint=0) +
      ggplot2::labs(title="K-Means Centroid Heatmap",
        subtitle="Centroids in the standardized technical-variable space",
        x=NULL,y="Exploratory technical profile",fill="Standardized\ncentroid",
        caption="Profiles are descriptive clusters, not validated risk levels.") +
      article_theme() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1))
    add(article_save(p,"Figure_KMeans_Centroids",9.5,5.5))
  }

  # 8 Quasi-Poisson IRR forest plot (or selected inferential count model if unavailable)
  cf <- r$tbl_coef
  if (!is.null(cf) && nrow(cf)) {
    fd <- cf %>% dplyr::filter(term!="(Intercept)",is.finite(estimate),is.finite(conf.low),is.finite(conf.high)) %>%
      dplyr::mutate(term=forcats::fct_reorder(term,estimate))
    if (nrow(fd)) {
      p <- ggplot2::ggplot(fd,ggplot2::aes(estimate,term)) +
        ggplot2::geom_vline(xintercept=1,linetype="dashed",color="gray45") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin=conf.low,xmax=conf.high),height=.18,color="#5D8AA8") +
        ggplot2::geom_point(size=2.6,color="#D77A7A") +
        ggplot2::scale_x_log10() +
        ggplot2::labs(title="Quasi-Poisson IRR Forest Plot",
          subtitle=paste("Inferential model:",r$tipo_mdl),
          x="Incidence Rate Ratio (log scale), 95% CI",y=NULL,
          caption="Poisson is used as the dispersion diagnostic reference; AIC/BIC are not used for Quasi-Poisson.") +
        article_theme()
      add(article_save(p,"Figure_QuasiPoisson_IRR",9.5,max(5.5,.28*nrow(fd)+2.5),interactive=TRUE))
    }
  }

  # 11 [v7.3.3] Sensitivity analyses (Figure 11 of the revised article)
  if (!is.null(r$sensibilidad)) {
    p <- tryCatch(plot_figure11(r$sensibilidad), error=function(e) NULL)
    if (!is.null(p)) add(article_save(p, "Figure_11_sensitivity", 11, 9.5))
  }

  # 9 optional organizational indicators, kept analytically separate
  if (isTRUE(r$tiene_encuesta) && !is.null(r$scores_enc) && nrow(r$scores_enc)) {
    od <- r$scores_enc %>% dplyr::select(FI,SI,IE,RD,PG,EIglobal) %>%
      tidyr::pivot_longer(dplyr::everything(),names_to="Indicator",values_to="Score")
    p <- ggplot2::ggplot(od,ggplot2::aes(Indicator,Score,fill=Indicator)) +
      ggplot2::geom_col(width=.65,show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",Score)),vjust=-.35,size=3.5) +
      ggplot2::scale_y_continuous(limits=c(0,5.35),breaks=0:5) +
      ggplot2::labs(title="Organizational Indicators",
        subtitle="Optional organizational module — analytically separate from the Technical Core",
        x=NULL,y="Mean score (1–5)",
        caption="FI, SI, IE, RD, PG and EIglobal are not inputs to PCA, K-Means or GLM.") +
      article_theme()
    add(article_save(p,"Figure_Organizational_Indicators",8.5,5.5))
  } else {
    add(tibble::tibble(Figure="Figure_Organizational_Indicators",PDF="Not available",
      JPG="Not available",HTML="Not applicable",Status="Skipped — optional questionnaire unavailable"))
  }
  dplyr::bind_rows(registry)
}

# Backward-compatible alias for the existing export button/server flow.
generar_publicacion <- generate_article_figures

# ── 3. UI ────────────────────────────────────────────────────
# CSS pastel para todo el dashboard (header, sidebar, cajas, valueBoxes)
css_pastel <- "
  /* Encabezado y sidebar en tonos pastel */
  .skin-blue .main-header .navbar,
  .skin-blue .main-header .logo { background-color:#A8D8EA; }
  .skin-blue .main-header .logo { color:#34495E; font-weight:bold; }
  .skin-blue .main-header .logo:hover { background-color:#97CBE0; }
  .skin-blue .main-header .navbar .sidebar-toggle { color:#34495E; }
  .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color:#97CBE0; }
  .skin-blue .main-sidebar { background-color:#4E6E81; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a {
    background-color:#3D5A6C; border-left-color:#F5B7B1; }
  .content-wrapper { background-color:#FBFCFC; }

  /* Bordes superiores pastel de las cajas */
  .box.box-primary { border-top-color:#85C1E9; }
  .box.box-info    { border-top-color:#76D7C4; }
  .box.box-success { border-top-color:#82E0AA; }
  .box.box-warning { border-top-color:#F8C471; }
  .box.box-danger  { border-top-color:#F1948A; }

  /* ValueBoxes en pastel con texto oscuro legible */
  .small-box.bg-blue,   .bg-blue   { background-color:#85C1E9 !important; color:#1B2631 !important; }
  .small-box.bg-red,    .bg-red    { background-color:#F1948A !important; color:#1B2631 !important; }
  .small-box.bg-green,  .bg-green  { background-color:#82E0AA !important; color:#1B2631 !important; }
  .small-box.bg-orange, .bg-orange { background-color:#F0B27A !important; color:#1B2631 !important; }
  .small-box.bg-yellow, .bg-yellow { background-color:#F9E79F !important; color:#1B2631 !important; }
  .small-box.bg-purple, .bg-purple { background-color:#C39BD3 !important; color:#1B2631 !important; }
  .small-box.bg-aqua,   .bg-aqua   { background-color:#76D7C4 !important; color:#1B2631 !important; }
  .small-box .icon { color:rgba(0,0,0,0.14) !important; }
  .small-box h3, .small-box p { color:#1B2631 !important; }

  /* Botones y pestañas suaves */
  .btn-primary { background-color:#85C1E9; border-color:#7FB3D5; color:#1B2631; }
  .btn-primary:hover { background-color:#7FB3D5; color:#1B2631; }
  .nav-tabs-custom > .nav-tabs > li.active { border-top-color:#F5B7B1; }

  /* Nota metodológica */
  .nota-metodo { background:#FEF9E7; border:1px solid #F9E79F; color:#7D6608;
                 padding:8px 12px; border-radius:6px; font-size:12px;
                 margin-bottom:12px; }
"

ui_build <- function(lang = APP_LANG_DEFAULT) dashboardPage(
  skin = "blue",
  dashboardHeader(title="Ciberseguridad Empresarial"),
  dashboardSidebar(
    div(style="padding:10px 15px 0 15px;",
        selectInput("app_lang", label=tags$span(icon("language"), "Idioma / Language"),
                    choices=APP_LANGS, selected=lang, width="100%")),
    sidebarMenu(
      menuItem("Carga",            tabName="carga",      icon=icon("upload")),
      menuItem("Technical Core",   tabName="kdd",        icon=icon("diagram-project")),
      if (MODULO_ORGANIZACIONAL) menuItem("Optional Organizational Module", tabName="encuesta",   icon=icon("clipboard-check")),
      menuItem("EDA",              tabName="eda",        icon=icon("chart-bar")),
      menuItem("ACP",              tabName="acp",        icon=icon("circle-nodes")),
      menuItem("Cluster",          tabName="cluster",    icon=icon("object-group")),
      menuItem("Modelo",           tabName="modelo",     icon=icon("calculator")),
      menuItem("Sensibilidad",     tabName="sensibilidad", icon=icon("scale-balanced")),
      menuItem("Resumen visual",   tabName="visual",     icon=icon("chart-pie")),
      menuItem("Proyección exploratoria",       tabName="prediccion", icon=icon("robot")),
      if (MODULO_ORGANIZACIONAL) menuItem("Minería de Texto", tabName="texto",      icon=icon("comments")),
      menuItem("Dashboard Final",  tabName="dashboard",  icon=icon("gauge-high")),
      menuItem("Conclusión",       tabName="conclusion", icon=icon("check-circle")),
      menuItem("Methodology / About", tabName="methodology", icon=icon("book-open")),
      if (MOSTRAR_PUBLICACION) menuItem("Publicación",      tabName="publicacion", icon=icon("newspaper"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(css_pastel))),
    tabItems(

      # CARGA ------------------------------------------------
      tabItem("carga",
        h2("Reusable Modular Analytical Framework"),
        div(class="nota-metodo",
          icon("circle-info"), " Arquitectura analítica: el ", strong("Technical Core"),
          " alimenta PCA, K-Means y el GLM. El ", strong("Optional Organizational Module"),
          " (FI, SI, IE, RD, PG, EIglobal) se analiza como evidencia complementaria ",
          "y no entra al modelo estadístico."),
        fluidRow(
          box(width=6, title="Input A — Required: Technical incident data", status="primary",
            downloadButton("dl_tech_template","Download official CSV template"),
            tags$span(" "),
            downloadButton("dl_dictionary","Download data dictionary"),
            br(), br(),
            fileInput("f_csv","Subir CSV",
                      accept=c(".csv","text/csv","text/plain")),
            helpText("Columnas esperadas: attack_type, target_system, outcome,",
                     "timestamp, data_compromised_GB, attack_duration_min,",
                     "security_tools_used, user_role, location, attack_severity,",
                     "industry, response_time_min, mitigation_method")
          ),
          if (MODULO_ORGANIZACIONAL) box(width=6, title="Input B — Optional: Organizational questionnaire", status="warning",
            downloadButton("dl_questionnaire","Download questionnaire template"),
            br(), br(),
            fileInput("f_xlsx","Subir XLSX",
                      accept=c(".xlsx",
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
            helpText("Google Forms exportado a Excel. 49 columnas,",
                     "escala Likert 5 niveles (FI, SI, IE, RD, PG),",
                     "4 preguntas abiertas, datos demográficos.")
          )
        ),
        fluidRow(
          box(width=12, status="info", title="Application mode",
            selectInput("analysis_mode","Analysis scope",
              choices=c("Auto-detect"="auto",
                        "Single organization"="single",
                        "Multi-organization / sector"="multi"),
              selected="auto"),
            helpText("Single organization emphasizes internal analysis; multi-organization enables aggregated sector comparison.")
          )
        ),
        uiOutput("carga_estado"),
        h3("Automatic validation — Technical Core"),
        DTOutput("validacion_tbl"),
        if (MODULO_ORGANIZACIONAL) h3("Automatic validation — Optional Organizational Module"),
        if (MODULO_ORGANIZACIONAL) DTOutput("validacion_enc_tbl"),
        h3("Vista previa del CSV (primeras 20 filas)"),
        DTOutput("tbl_preview")
      ),

      # KDD --------------------------------------------------
      tabItem("kdd",
        h2("Metodología KDD"),
        tabBox(width=12,
          tabPanel("1. Selección",
            h4("Dataset con columnas renombradas (primeras 30 filas)"),
            DTOutput("kdd1")),
          tabPanel("2. Preprocesamiento",
            h4("Valores faltantes"), DTOutput("kdd2_falt"),
            h4("Resumen de limpieza"), DTOutput("kdd2_res"),
            plotOutput("kdd2_plot")),
          tabPanel("3. Transformación",
            h4("Base agregada (año / mes / país / industria / tipo de ataque)"),
            DTOutput("kdd3"), plotOutput("kdd3_plot")),
          tabPanel("4. Minería",
            verbatimTextOutput("kdd4_diag"), DTOutput("kdd4_coef")),
          tabPanel("5. Interpretación",
            div(class="nota-metodo", icon("circle-info"), " ", CAT_FREC_TXT),
            DTOutput("kdd5_tbl"), plotOutput("kdd5_plot"))
        )
      ),

      # ENCUESTA ---------------------------------------------
      tabItem("encuesta",
        h2("Optional Organizational Module — Questionnaire"),
        div(class="nota-metodo",
          icon("circle-info"),
          " Este bloque es independiente del núcleo estadístico: las dimensiones ",
          "FI, SI, IE, RD, PG y EIglobal no participan en PCA, K-Means ni GLM."),
        fluidRow(
          box(width=12, title="Consistencia interna — Alpha de Cronbach",
              status="info", verbatimTextOutput("enc_alpha"))
        ),
        fluidRow(
          box(width=6, title="Respondentes",   status="primary",
              DTOutput("enc_demo")),
          box(width=6, title="Puntuación media por dimensión",
              status="success", plotOutput("enc_dim"))
        ),
        fluidRow(
          box(width=6, title="EIglobal (distribución)",
              status="warning", plotOutput("enc_hist")),
          box(width=6, title="EIglobal por sector",
              status="danger",  plotOutput("enc_sector"))
        ),
        fluidRow(
          box(width=12, title="Dimensiones por sector (detalle)",
              status="info", collapsible=TRUE,
              DTOutput("enc_sector_tbl"))
        )
      ),

      # EDA --------------------------------------------------
      tabItem("eda",
        h2("Análisis Exploratorio de Datos"),
        fluidRow(
          valueBoxOutput("vb_tot"), valueBoxOutput("vb_imp"),
          valueBoxOutput("vb_efx")
        ),
        fluidRow(
          valueBoxOutput("vb_sev"), valueBoxOutput("vb_exit"),
          valueBoxOutput("vb_resp")
        ),
        fluidRow(
          box(plotOutput("eda_anio"),   width=6, title="Incidentes por año"),
          box(plotOutput("eda_tipo"),   width=6, title="Por tipo de ataque")
        ),
        fluidRow(
          box(plotOutput("eda_ind"),    width=6, title="Impacto por industria"),
          box(plotOutput("eda_exito"),  width=6, title="Tasa de éxito por ataque")
        ),
        fluidRow(
          box(plotOutput("eda_sev"),    width=6, title="Distribución de severidad"),
          box(plotOutput("eda_mit"),    width=6, title="Métodos de mitigación")
        ),
        fluidRow(
          box(plotOutput("eda_sis"),    width=6, title="Sistemas objetivo"),
          box(plotOutput("eda_riesgo"), width=6,
              title="Categoría exploratoria de frecuencia ajustada")
        )
      ),

      # ACP --------------------------------------------------
      tabItem("acp",
        h2("Análisis de Componentes Principales"),
        div(class="nota-metodo",
          icon("circle-info"),
          " Calculado únicamente sobre las siete variables técnicas del dataset de incidentes."),
        fluidRow(
          box(plotOutput("acp_eig"),    width=6, title="Varianza explicada"),
          box(plotOutput("acp_biplot"), width=6, title="Biplot 2D")
        ),
        fluidRow(
          box(width=12, status="info", title="Interactive PCA 3D — PC1 / PC2 / PC3",
              plotly::plotlyOutput("acp_3d", height="620px"),
              uiOutput("acp_3d_note"))
        ),
        DTOutput("acp_tbl")
      ),

      # CLUSTER ----------------------------------------------
      tabItem("cluster",
        h2(paste0("Segmentación K-Means (k = ", K_FINAL, ")")),
        div(class="nota-metodo",
          icon("circle-info"),
          paste0(" Agrupamiento basado solo en variables técnicas. k = ", K_FINAL,
                 " se retuvo como solución final considerando el método del codo, el coeficiente ",
                 "Silhouette medio y la interpretabilidad de los perfiles; EIglobal se reporta como contraste ",
                 "externo, nunca como criterio de agrupamiento.")),
        fluidRow(
          valueBoxOutput("cluster_vb_sil_selected", width=4),
          valueBoxOutput("cluster_vb_sil_max", width=4),
          valueBoxOutput("cluster_vb_k", width=4)
        ),
        plotOutput("cluster_plot"),
        fluidRow(
          box(width=12, status="info", title="Interactive K-Means 3D — Exploratory Technical Profiles",
            fluidRow(
              column(2, uiOutput("cluster_x_ui")),
              column(2, uiOutput("cluster_y_ui")),
              column(2, uiOutput("cluster_z_ui")),
              column(2, uiOutput("cluster_profile_ui")),
              column(4,
                sliderInput("cluster_point_size","Point size",min=2,max=12,value=5,step=1),
                sliderInput("cluster_alpha","Opacity",min=.15,max=1,value=.70,step=.05))
            ),
            plotly::plotlyOutput("cluster_3d",height="650px"),
            helpText("Only existing technical variables are selectable. Profiles are exploratory; limited cluster separation is not hidden.")
          )
        ),
        h4("Caracterización técnica de clusters"),
        DTOutput("cluster_tbl"),
        h4("Diagnóstico reproducible de k"),
        DTOutput("cluster_kdiag_tbl")
      ),

      # MODELO -----------------------------------------------
      tabItem("modelo",
        h2("Modelo estadístico GLM"),
        box(width=12, status="primary",
            verbatimTextOutput("mdl_tipo")),
        box(width=12, status="info",
            verbatimTextOutput("mdl_res")),
        h4("Coeficientes (IRR con IC 95%)"),
        DTOutput("mdl_coef"),
        h4("Métricas globales"),
        DTOutput("mdl_metricas")
      ),

      # RESUMEN VISUAL [v7.3.3] ---------------------------------
      tabItem("visual",
        h2("Resumen visual"),
        div(class="nota-metodo", icon("circle-info"),
          " Superficie de respuesta del GLM, perfiles técnicos de K-Means y flujo de los terciles de prioridad ",
          "entre el modelo de impacto total y el de impacto medio. Son vistas exploratorias, no efectos causales."),
        box(width=12, status="primary", title="Resumen visual del análisis",
            helpText("Las descargas se generan siempre en inglés, como exige la revista."),
            plotOutput("vis_dashboard", height="900px"),
            downloadButton("dl_vis_pdf", "Descargar PDF (vectorial)"),
            downloadButton("dl_vis_jpg", "Descargar JPG (600 dpi)"))
      ),

      # SENSIBILIDAD [v7.3.3] -----------------------------------
      tabItem("sensibilidad",
        h2("Análisis de sensibilidad (revisión por pares)"),
        div(class="nota-metodo", icon("circle-info"),
          " Impacto total vs impacto medio, estabilidad de los terciles, PCA sin variables derivadas ",
          "y estabilidad de la silueta. Corresponden a la Sección 3.7, la Tabla 6, la Figura 11 y ",
          "las Tablas Suplementarias S1–S3 del artículo revisado."),
        box(width=12, status="primary", title="Figure 11 — Sensitivity analyses",
            helpText("Las descargas de la Figura 11 se generan siempre en inglés, como exige la revista."),
            plotOutput("sens_fig11", height="820px"),
            downloadButton("dl_fig11_pdf", "Descargar PDF (vectorial)"),
            downloadButton("dl_fig11_jpg", "Descargar JPG (600 dpi)")),
        fluidRow(
          box(width=6, status="info", title="Impacto total vs impacto medio", DTOutput("sens_impacto")),
          box(width=6, status="info", title="Estabilidad de los terciles de prioridad",
              DTOutput("sens_terciles"), br(), DTOutput("sens_tabla_terciles"))),
        fluidRow(
          box(width=6, status="info", title="Estabilidad de la silueta", DTOutput("sens_silueta")),
          box(width=6, status="info", title="PCA sin variables derivadas — congruencia de Tucker",
              DTOutput("sens_tucker"))),
        box(width=12, status="info", title="Varianza por componente en las tres especificaciones del PCA",
            DTOutput("sens_pca")),
        box(width=12, status="warning", title="Conway–Maxwell–Poisson (bajo demanda)",
            p("El modelo COM-Poisson puede tardar varios minutos con miles de celdas; se ejecuta solo al pulsar el botón. ",
              "Sus resultados se incluyen después en el reporte Excel."),
            actionButton("btn_cmp", "Ejecutar COM-Poisson", icon=icon("play")),
            br(), br(), DTOutput("sens_cmp_resumen"), br(), DTOutput("sens_cmp_comp"))
      ),

      # PROYECCIÓN -------------------------------------------
      tabItem("prediccion",
        tags$div(
          class="alert alert-warning",
          tags$b("Interpretación exploratoria: "),
          "La proyección representa una extrapolación exploratoria del GLM bajo la estructura observada de los datos. ",
          "No constituye un pronóstico temporal validado y debe interpretarse con cautela, especialmente cuando el periodo histórico disponible es limitado."
        ),

        h2("Proyección exploratoria de frecuencia de incidentes"),
        div(class="nota-metodo", icon("circle-info"), " ", CAT_FREC_PROY_TXT),

        # ── Selector de año a proyectar ──
        fluidRow(
          box(width=4, status="primary", title="¿Qué año desea proyectar?",
              icon("calendar-days"),
              uiOutput("pred_anio_ui"),
              helpText("El modelo GLM genera una extrapolación exploratoria de la frecuencia de incidentes",
                       "manteniendo la estructura de grupos del año base.",
                       "Las variaciones comparan predicción vs predicción",
                       "y los terciles usan puntos de corte fijos del año base.")),
          valueBoxOutput("fut_vb_total", width=4),
          valueBoxOutput("fut_vb_var",   width=4)
        ),
        fluidRow(
          valueBoxOutput("fut_vb_alto",   width=4),
          valueBoxOutput("fut_vb_ataque", width=4),
          valueBoxOutput("fut_vb_ind",    width=4)
        ),

        # ── Proyección visual ──
        fluidRow(
          box(width=6, status="primary",
              title="Tendencia histórica y proyección al año seleccionado",
              plotOutput("fut_trend", height="340px")),
          box(width=6, status="warning",
              title="Incidentes proyectados por tipo de ataque",
              plotOutput("fut_tipo", height="340px"))
        ),
        fluidRow(
          box(width=6, status="danger",
              title="Frecuencia proyectada por industria (terciles con referencia fija)",
              plotOutput("fut_ind", height="340px")),
          box(width=6, status="success",
              title="Recomendaciones para el año proyectado",
              uiOutput("fut_recs"))
        ),
        fluidRow(
          box(width=12, status="info", collapsible=TRUE,
              title="Detalle de la proyección por grupo",
              DTOutput("fut_tbl"))
        ),

        # ── Diagnóstico del modelo (ajuste histórico) ──
        h3("Diagnóstico del modelo (ajuste sobre datos históricos)"),
        fluidRow(
          box(plotOutput("pred_scatter"), width=6,
              title="Observados vs. ajustados"),
          box(plotOutput("pred_riesgo"),  width=6,
              title="Distribución por tercil de frecuencia ajustada")
        ),
        DTOutput("pred_tbl")
      ),

      # DASHBOARD FINAL --------------------------------------
      tabItem("dashboard",
        h2("Dashboard Final — Resumen ejecutivo del estudio"),
        fluidRow(
          valueBoxOutput("dsh_vb_periodo", width=3),
          valueBoxOutput("dsh_vb_inc",     width=3),
          valueBoxOutput("dsh_vb_imp",     width=3),
          valueBoxOutput("dsh_vb_modelo",  width=3)
        ),
        fluidRow(
          valueBoxOutput("dsh_vb_efx",   width=3),
          valueBoxOutput("dsh_vb_exito", width=3),
          valueBoxOutput("dsh_vb_resp",  width=3),
          valueBoxOutput("dsh_vb_alto",  width=3)
        ),
        fluidRow(
          box(width=8, status="primary",
              title="Evolución anual de incidentes por tipo de ataque",
              plotOutput("dsh_trend", height="330px")),
          box(width=4, status="warning",
              title="Composición por tercil de frecuencia ajustada",
              plotOutput("dsh_donut", height="330px"))
        ),
        fluidRow(
          box(width=4, status="danger",
              title="Top 5 vectores de ataque",
              plotOutput("dsh_top", height="300px")),
          box(width=4, status="info",
              title="Industrias más impactadas",
              plotOutput("dsh_ind", height="300px")),
          box(width=4, status="success",
              title="Perfiles técnicos (clusters K-Means)",
              plotOutput("dsh_clus", height="300px"))
        ),
        fluidRow(
          box(width=12, status="primary",
              title="Matriz de calor: severidad media por industria y tipo de ataque",
              plotOutput("dsh_heat", height="360px"))
        )
      ),

      # MINERÍA DE TEXTO -------------------------------------
      tabItem("texto",
        h2("Minería de texto y recomendaciones de expertos"),
        tabBox(width=12,
          tabPanel(icon=icon("lightbulb"),
            "Recomendaciones",
            uiOutput("rec_panel")),
          tabPanel(icon=icon("cloud"),
            "Nube de palabras",
            fluidRow(
              box(plotOutput("txt_cloud", height="420px"), width=6,
                  title="Nube de términos clave"),
              box(plotOutput("txt_bigram", height="420px"), width=6,
                  title="Bigramas más frecuentes")
            )
          ),
          tabPanel(icon=icon("table"),
            "Frecuencia de términos",
            DTOutput("txt_frec")),
          tabPanel(icon=icon("quote-left"),
            "Respuestas de expertos",
            DTOutput("txt_citas"))
        )
      ),

      # CONCLUSIÓN (visual + texto) ---------------------------
      tabItem("conclusion",
        h2("Conclusión del estudio"),
        fluidRow(
          valueBoxOutput("con_vb_modelo", width=3),
          valueBoxOutput("con_vb_inc",    width=3),
          valueBoxOutput("con_vb_efx",    width=3),
          valueBoxOutput("con_vb_exito",  width=3)
        ),
        if (MODULO_ORGANIZACIONAL) fluidRow(
          box(width=6, status="success",
              title="EIglobal — Optional Organizational Module (gauge)",
              plotOutput("con_gauge", height="320px")),
          box(width=6, status="warning",
              title="Distribución por tercil de frecuencia ajustada (donut)",
              plotOutput("con_donut", height="320px"))
        ) else fluidRow(
          box(width=6, status="warning",
              title="Distribución por tercil de frecuencia ajustada (donut)",
              plotOutput("con_donut", height="320px")),
          box(width=6, status="danger",
              title="Top 5 vectores de ataque",
              plotOutput("con_top", height="320px"))
        ),
        if (MODULO_ORGANIZACIONAL) fluidRow(
          box(width=6, status="primary",
              title="Dimensiones de efectividad (FI · SI · IE · RD · PG)",
              plotOutput("con_dims", height="320px")),
          box(width=6, status="danger",
              title="Top 5 vectores de ataque",
              plotOutput("con_top", height="320px"))
        ),
        fluidRow(
          box(width=12, status="info", collapsible=TRUE,
              title="Síntesis metodológica y estadística (texto)",
              verbatimTextOutput("conclusion"))
        )
      ),

      # PUBLICACIÓN (solo exportación, sin gráficos en pantalla)
      # METHODOLOGY / ABOUT --------------------------------------
      tabItem("methodology",
        h2("Methodology / About the model"),
        fluidRow(
          valueBox(paste0("v", FRAMEWORK_VERSION),
                   "Framework version", icon=icon("code-branch"), color="blue"),
          valueBox(as.character(RANDOM_SEED),
                   "Reproducibility seed", icon=icon("seedling"), color="green"),
          valueBox(paste0("k = ", K_FINAL),
                   "Final cluster solution", icon=icon("object-group"), color="purple")
        ),
        fluidRow(
          box(width=4, status="primary", solidHeader=TRUE,
              title="Technical Core — Required",
              p("Incident data are required and feed EDA, PCA, K-Means and the count GLM.")),
          box(width=4, status="warning", solidHeader=TRUE,
              title="Organizational Module — Optional",
              p("The questionnaire produces FI, SI, IE, RD, PG and EIglobal as complementary evidence only.")),
          box(width=4, status="success", solidHeader=TRUE,
              title="Strict analytical separation",
              p("EIglobal and all questionnaire dimensions do not enter PCA, K-Means or GLM."))
        ),
        box(width=12, status="info", title="Reproducibility statement",
            p(K_FINAL_RATIONALE),
            p("Questionnaire validation excludes incomplete individual responses without blocking the optional module when valid respondents remain."),
            p(paste0("K-Means random seed: ", RANDOM_SEED,
                     ". Model/framework version: ", FRAMEWORK_VERSION, ".")),
            p(CAT_FREC_TXT),
            p("Future estimates are presented as exploratory model-based extrapolations rather than validated time-series forecasts.")),
        fluidRow(
          box(width=12, status="primary",
              title="Export",
              downloadButton("dl_analysis_report",
                             "Download analysis report (.xlsx)",
                             icon=icon("download")))
        )
      ),

      if (MOSTRAR_PUBLICACION) tabItem("publicacion",
        h2("Publicación — Exportación de gráficos académicos"),
        helpText("Las figuras del artículo se exportan siempre en inglés, como exige la revista."),
        fluidRow(
          box(width=12, status="primary",
              title="Generar y guardar gráficos (PDF · JPG · HTML)",
            p("Los gráficos de publicación ", strong("no se muestran en el Shiny"),
              ": se generan en calidad de impresión y se guardan automáticamente en:"),
            tags$ul(
              tags$li(code("outputs/article_figures/pdf/"),  " — versión vectorial (PDF)"),
              tags$li(code("outputs/article_figures/jpg/"),  " — imagen 300 dpi (JPG)"),
              tags$li(code("outputs/article_figures/html/"), " — página HTML autocontenida")
            ),
            actionButton("btn_publicar",
                         "Generar y guardar gráficos de publicación",
                         icon=icon("file-export"),
                         class="btn-primary btn-lg")
          )
        ),
        uiOutput("pub_estado"),
        fluidRow(
          box(width=12, status="info",
              title="Registro de archivos generados",
              DTOutput("pub_archivos"))
        )
      ) else tabItem("publicacion")

    )
  )
)

# ── 4. SERVER ────────────────────────────────────────────────
# [v7.3.3] La interfaz se construye en el idioma pedido en la URL (?lang=es|en)
ui <- function(request) {
  lang <- i18n_lang_from_query(request$QUERY_STRING)
  i18n_set_lang(lang)
  tr_tag(ui_build(lang))
}

server <- function(input, output, session) {
  # [v7.3.3] Idioma de esta sesión y cambio de idioma (recarga la app)
  i18n_set_lang(i18n_lang_from_query(isolate(session$clientData$url_search)))
  observeEvent(input$app_lang, {
    if (!identical(input$app_lang, i18n_lang())) {
      updateQueryString(paste0("?lang=", input$app_lang), mode="replace")
      session$reload()
    }
  }, ignoreInit=TRUE)
  cmp_res <- reactiveVal(NULL)   # [v7.3.3] resultado del COM-Poisson bajo demanda

  # ── Reactivos base ────────────────────────────────────────
  raw_csv <- reactive({
    req(input$f_csv)
    tryCatch(
      read_csv(input$f_csv$datapath, show_col_types=FALSE),
      error=function(e) {
        showNotification(paste("Error al leer CSV:", e$message),
                         type="error", duration=10)
        NULL
      }
    )
  })

  raw_xlsx <- reactive({
    if (is.null(input$f_xlsx)) return(NULL)
    tryCatch(
      readxl::read_excel(input$f_xlsx$datapath),
      error=function(e) {
        showNotification(paste("Error al leer XLSX:", e$message),
                         type="error", duration=10)
        NULL
      }
    )
  })

  validacion_csv <- reactive({
    req(raw_csv())
    validar_incidentes(raw_csv())
  })

  validacion_enc <- reactive({
    validar_cuestionario(raw_xlsx())
  })

  xlsx_validado <- reactive({
    x <- raw_xlsx()
    if (is.null(x)) return(NULL)
    vq <- validacion_enc()
    if (!isTRUE(vq$ok)) return(NULL)
    x
  })

  res <- reactive({
    req(raw_csv())
    v <- validacion_csv()
    validate(need(isTRUE(v$ok),
      paste(c("Technical input validation failed.", v$errors), collapse=" ")))
    withProgress(message="Procesando datos…", value=0, {
      tryCatch(
        procesar_kdd(raw_csv(), xlsx_validado(), input$analysis_mode),
        error=function(e) {
          showNotification(
            paste("Error en KDD:", conditionMessage(e)),
            type="error", duration=15)
          NULL
        }
      )
    })
  })

  output$dl_tech_template <- downloadHandler(
    filename=function() "technical_incident_template.csv",
    content=function(file) {
      x <- as.data.frame(matrix(nrow=0, ncol=length(c(TECH_REQUIRED,TECH_OPTIONAL_META))))
      names(x) <- c(TECH_REQUIRED,TECH_OPTIONAL_META)
      readr::write_csv(x, file)
    }
  )

  output$dl_dictionary <- downloadHandler(
    filename=function() "technical_data_dictionary.csv",
    content=function(file) readr::write_csv(diccionario_tecnico(), file)
  )

  output$dl_questionnaire <- downloadHandler(
    filename=function() "optional_organizational_questionnaire_template.xlsx",
    content=function(file) {
      likert <- tibble::tibble(
        value=1:5,
        label=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
      )
      info <- tibble::tibble(
        note=c(
          "20 Likert items: FI/SI/IE/RD/PG, four items each.",
          "respondent_type: Internal or External expert.",
          "For single-organization mode use Internal for employees and fill organization_id/business_unit.",
          "For multi-organization/sector mode fill industry and anonymized organization_id where available.",
          "Open questions are optional."
        )
      )
      writexl::write_xlsx(
        list(Questionnaire=plantilla_cuestionario(),
             Likert_scale=likert,
             Instructions=info),
        path=file
      )
    }
  )

  output$dl_analysis_report <- downloadHandler(
    filename=function() {
      paste0("cybersecurity_framework_v", FRAMEWORK_VERSION,
             "_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content=function(file) {
      r <- res()
      req(r)

      pca_tbl <- if (!is.null(r$pca_res)) {
        tibble::tibble(
          component=paste0("PC", seq_along(r$pca_res$var_pct)),
          variance_pct=r$pca_res$var_pct,
          cumulative_pct=r$pca_res$var_cum
        )
      } else tibble::tibble(info="PCA not available")

      disp_tbl <- tibble::tibble(
        metric=c("dispersion_ratio","pearson_chisq","df_residual",
                 "p_under","p_over","status","selected_model"),
        value=as.character(c(
          r$dispersion_diag$dispersion_ratio,
          r$dispersion_diag$pearson_chisq,
          r$dispersion_diag$df_residual,
          r$dispersion_diag$p_under,
          r$dispersion_diag$p_over,
          r$dispersion_diag$status,
          r$tipo_mdl
        ))
      )

      q_summary <- if (!is.null(r$scores_enc))
        as.data.frame(r$scores_enc)
      else
        data.frame(Status="Organizational questionnaire not provided/valid — optional module.")

      q_sector <- if (!is.null(r$scores_sector))
        as.data.frame(r$scores_sector)
      else
        data.frame(Status="Not available")

      session_tbl <- data.frame(
        session_info=capture.output(utils::sessionInfo()),
        stringsAsFactors=FALSE
      )

      meta_tbl <- data.frame(
        item=c(
          "Framework version","Random seed","Final k",
          "k selection rationale","Technical Core",
          "Organizational Module","Separation rule","Frequency categories","Projection interpretation"
        ),
        value=c(
          FRAMEWORK_VERSION,
          as.character(RANDOM_SEED),
          as.character(r$k_selected),
          K_FINAL_RATIONALE,
          "Required: EDA + PCA + K-Means + count GLM",
          "Optional: FI/SI/IE/RD/PG/EIglobal",
          "EIglobal and questionnaire dimensions do not enter PCA/K-Means/GLM",
          "Bajo/Medio/Alto are exploratory fitted-frequency terciles, not risk levels",
          "Future estimates are presented as exploratory model-based extrapolations rather than validated time-series forecasts."
        ),
        stringsAsFactors=FALSE
      )

      sheets <- list(
        Framework_meta     = meta_tbl,
        Technical_summary  = as.data.frame(r$resumen_mdl),
        K_diagnostic       = as.data.frame(r$k_diag),
        Cluster_profiles   = as.data.frame(r$cluster_profiles),
        GLM_coefficients   = as.data.frame(r$tbl_coef),
        GLM_dispersion     = disp_tbl,
        PCA_variance       = pca_tbl,
        Silhouette_full    = data.frame(
          silhouette_selection_subsample = r$k_selected_sil,
          silhouette_final_partition_all_cells = r$sil_final_full),
        Sens_impact        = if (!is.null(r$sensibilidad)) r$sensibilidad$resumen_impacto else data.frame(info="Not available"),
        Sens_mean_impact_coef = if (!is.null(r$sensibilidad)) r$sensibilidad$coef_impacto_medio else data.frame(info="Not available"),
        Sens_tier_stability = if (!is.null(r$sensibilidad)) r$sensibilidad$estabilidad_terciles else data.frame(info="Not available"),
        Sens_tier_crosstab = if (!is.null(r$sensibilidad)) r$sensibilidad$tabla_terciles else data.frame(info="Not available"),
        Sens_PCA_variance  = if (!is.null(r$sensibilidad)) r$sensibilidad$pca_var else data.frame(info="Not available"),
        Sens_PCA_loadings  = if (!is.null(r$sensibilidad)) r$sensibilidad$pca_cargas else data.frame(info="Not available"),
        Sens_PCA_tucker    = if (!is.null(r$sensibilidad)) r$sensibilidad$tucker else data.frame(info="Not available"),
        Sens_silhouette    = if (!is.null(r$sensibilidad)) r$sensibilidad$silueta else data.frame(info="Not available"),
        Sens_COM_Poisson   = if (!is.null(cmp_res())) cmp_res()$resumen else data.frame(info="Run COM-Poisson in the Sensibilidad tab"),
        Sens_COM_Poisson_vs_main = if (!is.null(cmp_res())) cmp_res()$comparacion else data.frame(info="Run COM-Poisson in the Sensibilidad tab"),
        Questionnaire      = q_summary,
        Questionnaire_sector = q_sector,
        Recommendations    = as.data.frame(recomendaciones_framework(r)),
        Session_info       = session_tbl
      )

      writexl::write_xlsx(sheets, path=file)
    }
  )

  # ── CARGA ─────────────────────────────────────────────────
  output$carga_estado <- renderUI({
    if (is.null(raw_csv())) return(NULL)
    v <- validacion_csv()
    mode <- detectar_modo(raw_csv(), input$analysis_mode)
    tagList(
      tags$div(class=if (v$ok) "alert alert-success" else "alert alert-danger",
        icon(if (v$ok) "check" else "triangle-exclamation"),
        if (v$ok)
          sprintf(" Technical input valid: %s rows × %s columns | Mode: %s",
                  scales::comma(nrow(raw_csv())), ncol(raw_csv()), mode)
        else paste(" Validation blocked KDD:", paste(v$errors, collapse=" | "))),
      if (length(v$warnings))
        tags$div(class="alert alert-warning",
          paste("Warnings:", paste(v$warnings, collapse=" | "))),
      if (MODULO_ORGANIZACIONAL) {
        vq <- validacion_enc()
        status_class <- if (!isTRUE(vq$provided)) {
          "alert alert-info"
        } else if (!isTRUE(vq$ok)) {
          "alert alert-danger"
        } else if (length(vq$warnings) > 0) {
          "alert alert-warning"
        } else {
          "alert alert-success"
        }

        status_text <- if (!isTRUE(vq$provided)) {
          "Organizational questionnaire not provided — optional module."
        } else if (!isTRUE(vq$ok)) {
          paste0(
            "Questionnaire validation failed — organizational module will be omitted: ",
            paste(vq$errors, collapse=" | "))
        } else if (length(vq$warnings) > 0) {
          paste0(
            "Optional Organizational Module available: ",
            vq$n_valid, " valid response(s) retained",
            if (vq$n_excluded > 0)
              paste0("; ", vq$n_excluded, " incomplete response(s) excluded")
            else "",
            ". ", paste(vq$warnings, collapse=" | "))
        } else {
          paste0(
            "Optional Organizational Module valid: ",
            vq$n_valid, " response(s) retained.")
        }

        tags$div(class=status_class, status_text)
      }
    )
  })

  output$validacion_tbl <- renderDT({
    req(raw_csv())
    datatable(validacion_csv()$summary,
      caption="Missingness audit before KDD",
      options=list(pageLength=20, scrollX=TRUE), rownames=FALSE)
  })

  output$validacion_enc_tbl <- renderDT({
    vq <- validacion_enc()
    if (!isTRUE(vq$provided)) {
      return(datatable(
        data.frame(Status="Organizational questionnaire not provided — optional module."),
        options=list(dom="t"), rownames=FALSE))
    }
    d <- vq$summary
    # [v7.3.3] La columna `value` mezcla números y texto: se unifica como
    # texto para que bind_rows() no falle al añadir errores o advertencias.
    if (!is.null(d) && nrow(d) && "value" %in% names(d)) d$value <- as.character(d$value)
    if (length(vq$errors))
      d <- dplyr::bind_rows(
        d,
        tibble::tibble(metric="Validation errors",
                       value=paste(vq$errors, collapse=" | "))
      )
    if (length(vq$warnings))
      d <- dplyr::bind_rows(
        d,
        tibble::tibble(metric="Non-blocking warnings",
                       value=paste(vq$warnings, collapse=" | "))
      )
    datatable(d, options=list(dom="t", scrollX=TRUE), rownames=FALSE)
  })

  output$tbl_preview <- renderDT({
    req(raw_csv())
    datatable(head(raw_csv(), 20),
              options=list(scrollX=TRUE, pageLength=10))
  })

  # ── KDD ───────────────────────────────────────────────────
  output$kdd1 <- renderDT({
    req(res())
    datatable(head(res()$datos_orig, 30),
              options=list(scrollX=TRUE, pageLength=10))
  })

  output$kdd2_falt <- renderDT({
    req(res())
    datatable(res()$tbl_faltantes)
  })

  output$kdd2_res <- renderDT({
    req(res())
    datatable(res()$resumen_limpie)
  })

  output$kdd2_plot <- renderPlot({
    req(res())
    d <- res()$tbl_faltantes
    if (nrow(d) == 0) {
      p <- ggplot(data.frame(x=0.5, y=0.5, msg="Sin valores faltantes en el dataset"),
                  ggplot2::aes(x=x, y=y, label=msg)) +
        geom_text(size=5, color="#7FB3D5") + theme_void()
    } else {
      p <- ggplot(d, aes(reorder(variable,valores_faltantes),
                         valores_faltantes, fill=valores_faltantes)) +
        geom_col(show.legend=FALSE) + coord_flip() +
        scale_fill_gradient(low="#AED6F1", high="#F1948A") +
        labs(title="Valores faltantes", x=NULL, y="N") +
        theme_minimal()
    }
    guardar_plot(p, "kdd2_faltantes"); p
  })

  output$kdd3 <- renderDT({
    req(res())
    datatable(res()$base, options=list(scrollX=TRUE, pageLength=10))
  })

  output$kdd3_plot <- renderPlot({
    req(res())
    p <- res()$base %>%
      group_by(anio) %>%
      summarise(total=sum(n_incidentes),.groups="drop") %>%
      ggplot(aes(factor(anio), total, fill=factor(anio))) +
      geom_col(show.legend=FALSE) +
      scale_fill_brewer(palette="Pastel1") +
      labs(title="Incidentes por año", x="Año", y="Total") +
      theme_minimal()
    guardar_plot(p,"kdd3_anio"); p
  })

  output$kdd4_diag <- renderPrint({
    req(res())
    cat("Modelo seleccionado:", res()$tipo_mdl, "\n")
    d <- res()$dispersion_diag
    cat("Ratio de dispersión de Pearson:", round(d$dispersion_ratio, 3), "\n")
    cat("Estado:", d$status, "\n")
    cat("p subdispersión:", signif(d$p_under, 4),
        "| p sobredispersión:", signif(d$p_over, 4), "\n")
    cat("Criterio: pruebas direccionales basadas en las colas inferior y superior de X² de Pearson, con α = 0.05.\n")
    if (identical(res()$tipo_mdl, "Quasi-Poisson"))
      cat("Nota: Quasi-Poisson corrige la inferencia bajo subdispersión; AIC/BIC no están definidos.\n")
    cat("\nVariables del espacio multivariado (PCA / K-Means):\n  ")
    cat(paste(res()$vars_cols, collapse=", "), "\n")
    cat("\nk utilizado:", res()$k_selected, "\n")
    cat("  k = 3 se retuvo como solución final considerando conjuntamente\n")
    cat("  el método del codo, el coeficiente Silhouette medio y la\n")
    cat("  interpretabilidad de los perfiles obtenidos.\n")
    cat("  El máximo empírico de Silhouette puede no coincidir con este valor.\n")
  })
  
  
  
  
  
  

  output$kdd4_coef <- renderDT({
    req(res())
    datatable(res()$tbl_coef, options=list(scrollX=TRUE))
  })

  output$kdd5_tbl <- renderDT({
    req(res())
    res()$base_res %>%
      dplyr::select(anio, mes, pais, industria, tipo_ataque,
             n_incidentes, ajustados, error_pred, cat_frec_ajustada) %>%
      dplyr::rename(`cat_frec_ajustada (tercil)` = cat_frec_ajustada) %>%
      mutate(across(where(is.numeric), ~round(.,2))) %>%
      datatable(caption=CAT_FREC_TXT,
                options=list(scrollX=TRUE, pageLength=10))
  })

  output$kdd5_plot <- renderPlot({
    req(res())
    p <- res()$base_res %>%
      ggplot(aes(n_incidentes, ajustados)) +
      geom_point(alpha=.5, color="#85C1E9") +
      geom_abline(linetype="dashed", color="#F1948A") +
      labs(title="Observados vs ajustados", x="Observado", y="Ajustado") +
      theme_minimal()
    guardar_plot(p,"kdd5_pred"); p
  })

  # ── OPTIONAL ORGANIZATIONAL MODULE ────────────────────────
  output$enc_alpha <- renderPrint({
    req(res())
    if (is.null(res()$alpha_enc)) {
      cat("Organizational questionnaire not provided — optional module.\n")
    } else {
      cat("=== Alpha de Cronbach ===\n\n")
      print(res()$alpha_enc$total)
    }
  })

  output$enc_demo <- renderDT({
    req(res())
    if (is.null(res()$enc_limpia)) {
      datatable(data.frame(
        mensaje="Organizational questionnaire not provided — optional module."))
    } else {
      res()$enc_limpia %>%
        dplyr::select(dplyr::any_of(c("genero","empresa","cargo","experiencia","participa","respondent_type","sector","EIglobal"))) %>%
        mutate(EIglobal=round(EIglobal,2)) %>%
        datatable(options=list(scrollX=TRUE))
    }
  })

  output$enc_sector_tbl <- renderDT({
    req(res())
    if (is.null(res()$scores_sector)) {
      return(datatable(data.frame(
        Nota="Organizational questionnaire not provided — optional module.")))
    }
    res()$scores_sector %>%
      dplyr::mutate(across(where(is.numeric), ~round(.,2))) %>%
      datatable(
        caption="Dimensiones organizacionales por sector (bloque complementario, fuera del modelo). Sin imputación: los sectores sin respondentes no aparecen.",
        options=list(scrollX=TRUE), rownames=FALSE)
  })

  enc_dims <- reactive({
    req(res())
    if (is.null(res()$enc_limpia)) return(NULL)
    tibble(
      Dimensión = c("FI\nFrecuencia","SI\nSeveridad",
                    "IE\nEcon.","RD\nRespuesta","PG\nGlobal"),
      Media = c(
        mean(res()$enc_limpia$FI, na.rm=TRUE),
        mean(res()$enc_limpia$SI, na.rm=TRUE),
        mean(res()$enc_limpia$IE, na.rm=TRUE),
        mean(res()$enc_limpia$RD, na.rm=TRUE),
        mean(res()$enc_limpia$PG, na.rm=TRUE)
      )
    )
  })

  # Leyenda de procedencia de la evidencia organizacional
  cap_enc <- reactive({
    req(res())
    if (isTRUE(res()$tiene_encuesta))
      "Fuente: cuestionario Likert 1–5 — evidencia organizacional externa al núcleo técnico"
    else
      "Organizational questionnaire not provided — optional module"
  })

  output$enc_dim <- renderPlot({
    req(res())
    if (is.null(enc_dims())) return(plot_msg("Organizational questionnaire not provided — optional module"))
    p <- ggplot(enc_dims(), aes(Dimensión, Media, fill=Dimensión)) +
      geom_col(show.legend=FALSE) +
      geom_hline(yintercept=3, linetype="dashed", color="gray55") +
      scale_y_continuous(limits=c(0,5), breaks=1:5) +
      scale_fill_brewer(palette="Pastel2") +
      labs(title="Puntuación media por dimensión (1–5)",
           x=NULL, y="Media", caption=cap_enc()) +
      theme_minimal() +
      theme(plot.caption=element_text(color="gray55", hjust=0))
    guardar_plot(p,"enc_dim"); p
  })

  output$enc_hist <- renderPlot({
    req(res())
    if (is.null(res()$enc_limpia))
      return(plot_msg("Organizational questionnaire not provided — optional module"))
    d <- res()$enc_limpia$EIglobal
    p <- ggplot(data.frame(x=d), aes(x)) +
      geom_histogram(bins=10, fill="#AED6F1", color="white") +
      geom_vline(xintercept=mean(d,na.rm=TRUE),
                 color="#F1948A", linewidth=1, linetype="dashed") +
      labs(title="Distribución de EIglobal",
           x="EIglobal (1–5)", y="Frecuencia", caption=cap_enc()) +
      theme_minimal() +
      theme(plot.caption=element_text(color="gray55", hjust=0))
    guardar_plot(p,"enc_hist"); p
  })

  output$enc_sector <- renderPlot({
    req(res())
    if (is.null(res()$scores_sector))
      return(plot_msg("Organizational questionnaire not provided — optional module"))
    d <- res()$scores_sector %>%
      dplyr::transmute(sector=industria, media=EIglobal, n=n_respondentes)
    p <- ggplot(d, aes(reorder(sector,media), media, fill=media)) +
      geom_col(show.legend=FALSE) + coord_flip() +
      scale_fill_gradient(low="#F5B7B1", high="#AED6F1") +
      scale_y_continuous(limits=c(0,5)) +
      labs(title=if (res()$analysis_mode=="single") "EIglobal — internal organizational evidence"
           else "EIglobal by sector",
           x=NULL, y="Media (1–5)", caption=cap_enc()) +
      theme_minimal() +
      theme(plot.caption=element_text(color="gray55", hjust=0))
    guardar_plot(p,"enc_sector"); p
  })

  # ── EDA ───────────────────────────────────────────────────
  lbl_bar <- function(n, total=NULL) {
    if (is.null(total)) return(scales::comma(n))
    paste0(scales::comma(n),"\n(",round(n/total*100,1),"%)")
  }

  output$vb_tot <- renderValueBox({
    req(res())
    valueBox(scales::comma(nrow(res()$datos_limpios)),
             "Total incidentes analizados",
             icon=icon("shield-halved"), color="blue")
  })
  output$vb_imp <- renderValueBox({
    req(res())
    valueBox(
      paste0(round(sum(res()$base$impacto_total)/1e6,1)," M"),
      "Impacto económico total (escala)", icon=icon("dollar-sign"), color="red")
  })
  output$vb_efx <- renderValueBox({
    req(res())
    valueBox(
      round(mean(res()$base$idx_resiliencia,na.rm=TRUE),2),
      "Resiliencia técnica media (0–100)",
      icon=icon("chart-line"), color="green")
  })
  output$vb_sev <- renderValueBox({
    req(res())
    valueBox(
      round(mean(res()$datos_limpios$severidad,na.rm=TRUE),1),
      "Severidad media de ataques (1–10)",
      icon=icon("triangle-exclamation"), color="orange")
  })
  output$vb_exit <- renderValueBox({
    req(res())
    pct <- mean(res()$datos_limpios$exito,na.rm=TRUE)
    valueBox(
      scales::percent(pct, accuracy=0.1),
      "Tasa de ataques exitosos",
      icon=icon("skull-crossbones"), color="red")
  })
  output$vb_resp <- renderValueBox({
    req(res())
    valueBox(
      round(mean(res()$datos_limpios$t_respuesta_min,na.rm=TRUE),0),
      "Tiempo medio de respuesta (min)",
      icon=icon("clock"), color="yellow")
  })

  output$eda_anio <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::count(anio, tipo_ataque) %>%
      dplyr::group_by(anio) %>%
      dplyr::mutate(pct=round(n/sum(n)*100,1)) %>%
      dplyr::ungroup()
    tot <- res()$datos_limpios %>% dplyr::count(anio) %>%
      dplyr::mutate(label=paste0("n=",scales::comma(n)))
    p <- ggplot2::ggplot(d, ggplot2::aes(factor(anio), n, fill=tipo_ataque)) +
      ggplot2::geom_col(position="stack") +
      ggplot2::geom_text(data=tot,
                         ggplot2::aes(x=factor(anio), y=n, label=label, fill=NULL),
                         vjust=-0.4, size=3.5, fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque, name="Tipo de ataque") +
      ggplot2::labs(
        title="Incidentes por año y tipo de ataque",
        subtitle="Barras apiladas: cada color = tipo de ataque",
        x="Año", y="Número de incidentes",
        caption="Fuente: datos técnicos de incidentes") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(legend.position="bottom",
                     plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_anio"); p
  })

  output$eda_tipo <- renderPlot({
    req(res())
    total <- nrow(res()$datos_limpios)
    d <- res()$datos_limpios %>%
      dplyr::count(tipo_ataque, sort=TRUE) %>%
      dplyr::mutate(pct=round(n/total*100,1),
                    label=paste0(scales::comma(n)," (",pct,"%)"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(tipo_ataque,n), n, fill=tipo_ataque)) +
      ggplot2::geom_col(show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.05, size=3.5, color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque) +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.3))) +
      ggplot2::labs(
        title="Frecuencia por tipo de ataque",
        subtitle="Etiquetas: total absoluto y porcentaje sobre el total de incidentes",
        x=NULL, y="Número de incidentes",
        caption=paste0("Total analizado: ",scales::comma(total)," incidentes")) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_tipo"); p
  })

  output$eda_ind <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(
        n=dplyr::n(),
        impacto=sum(impacto,na.rm=TRUE)/1e6,
        sev=mean(severidad,na.rm=TRUE),
        .groups="drop"
      ) %>%
      dplyr::arrange(desc(impacto)) %>%
      dplyr::mutate(label=paste0(round(impacto,1),"M\n(n=",scales::comma(n),")"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(industria,impacto), impacto, fill=sev)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.05, size=3.2, color="#4A4A4A") +
      ggplot2::scale_fill_gradient(low="#AED6F1", high="#F1948A",
                                   name="Severidad\nmedia") +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.35))) +
      ggplot2::labs(
        title="Impacto económico por industria",
        subtitle="Color = severidad media del ataque (azul=bajo, rosado=alto)",
        x=NULL, y="Impacto total (millones, escala proporcional)",
        caption="Impacto = GB comprometidos × severidad × 1,000") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_industria"); p
  })

  output$eda_exito <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::group_by(tipo_ataque) %>%
      dplyr::summarise(
        tasa=mean(exito,na.rm=TRUE),
        n=dplyr::n(), .groups="drop"
      ) %>%
      dplyr::mutate(label=paste0(round(tasa*100,1),"%"))
    media_global <- mean(res()$datos_limpios$exito, na.rm=TRUE)
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(tipo_ataque,tasa), tasa, fill=tasa)) +
      ggplot2::geom_col(show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.1, size=3.5, color="#4A4A4A") +
      ggplot2::geom_hline(yintercept=media_global, linetype="dashed",
                          color="gray40", linewidth=0.8) +
      ggplot2::annotate("text", x=0.6, y=media_global+0.01,
                        label=paste0("Media global: ",round(media_global*100,1),"%"),
                        size=3, color="gray40", hjust=0) +
      ggplot2::scale_fill_gradient(low="#A9DFBF", high="#F1948A") +
      ggplot2::scale_y_continuous(labels=scales::percent_format(),
                                  expand=ggplot2::expansion(mult=c(0,.2))) +
      ggplot2::coord_flip(clip="off") +
      ggplot2::labs(
        title="Tasa de éxito del ataque por tipo",
        subtitle="Verde=bajo éxito (buena defensa), Rosado=alto éxito (mayor exposición)",
        x=NULL, y="Tasa de éxito (%)",
        caption="Éxito = ataque con outcome=Success") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_exito"); p
  })

  output$eda_sev <- renderPlot({
    req(res())
    med <- mean(res()$datos_limpios$severidad, na.rm=TRUE)
    p <- ggplot2::ggplot(res()$datos_limpios, ggplot2::aes(x=severidad)) +
      ggplot2::geom_histogram(bins=10, fill="#AED6F1", color="white", alpha=0.95) +
      ggplot2::geom_vline(xintercept=med, color="#F1948A",
                          linewidth=1.2, linetype="dashed") +
      ggplot2::annotate("text", x=med+0.3, y=Inf,
                        label=paste0(tr_exact("Media="),round(med,2)),
                        vjust=1.5, color="#F1948A", size=4, hjust=0) +
      ggplot2::scale_x_continuous(breaks=1:10) +
      ggplot2::labs(
        title="Distribución de severidad de los ataques",
        subtitle="Escala 1 (leve) – 10 (crítico). Línea rosada = media global",
        x="Nivel de severidad (1–10)", y="Número de incidentes",
        caption="Verifique la forma de la distribución antes de interpretar la media") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_sev"); p
  })

  output$eda_mit <- renderPlot({
    req(res())
    total <- nrow(res()$datos_limpios)
    d <- res()$datos_limpios %>%
      dplyr::count(mitigacion, sort=TRUE) %>%
      dplyr::mutate(pct=round(n/total*100,1),
                    label=paste0(scales::comma(n)," (",pct,"%)"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(mitigacion,n), n, fill=mitigacion)) +
      ggplot2::geom_col(show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.05, size=3.5, color="#4A4A4A") +
      ggplot2::scale_fill_brewer(palette="Pastel2") +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.3))) +
      ggplot2::labs(
        title="Métodos de mitigación utilizados",
        subtitle="Etiquetas: total absoluto y proporción del total de incidentes",
        x=NULL, y="Número de incidentes",
        caption="Containment=contención, Patch=parche, Quarantine=cuarentena") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_mit"); p
  })

  output$eda_sis <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::count(sistema, sort=TRUE) %>%
      dplyr::mutate(
        pct=round(n/sum(n)*100,1),
        label=paste0(scales::comma(n)," (",pct,"%)"),
        exposicion=ifelse(n > mean(n)*1.1,"Sobre la media","Normal")
      )
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(sistema,n), n, fill=exposicion)) +
      ggplot2::geom_col(show.legend=TRUE) +
      ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.05, size=3.5, color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=c(`Sobre la media`="#F5B7B1",Normal="#AED6F1"),
                                  name="Frecuencia") +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.3))) +
      ggplot2::labs(
        title="Sistemas más atacados",
        subtitle="Rosado = sistemas con frecuencia superior a la media (mayor exposición observada)",
        x=NULL, y="Número de ataques recibidos",
        caption="API=Interfaz de Programación, IoT=Internet de las Cosas") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
    guardar_plot(p,"eda_sis"); p
  })

  output$eda_riesgo <- renderPlot({
    req(res())
    d <- res()$base_res %>%
      dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(
        pct=round(n/sum(n)*100,1),
        label=paste0(n,"\n(",pct,"%)")
      )
    p <- ggplot2::ggplot(d, ggplot2::aes(cat_frec_ajustada, n, fill=cat_frec_ajustada)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.6) +
      ggplot2::geom_text(ggplot2::aes(label=label), vjust=-0.3, size=4,
                         fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_riesgo) +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.2))) +
      ggplot2::labs(
        title="Distribución de la categoría exploratoria de frecuencia ajustada",
        subtitle="Terciles de la frecuencia de incidentes ajustada por el modelo GLM",
        x="Tercil de frecuencia ajustada",
        y="Número de grupos (año×país×industria×ataque)",
        caption=CAT_FREC_TXT) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"eda_cat_frec"); p
  })

  # ── ACP ───────────────────────────────────────────────────
  # ── INTERACTIVE PCA / K-MEANS (Shiny only) ─────────────────────────
  output$acp_3d <- plotly::renderPlotly(tr_plotly({
    req(res())
    pca <- res()$pca_res
    validate(need(!is.null(pca) && ncol(pca$x) >= 3,
                  "At least three principal components are required."))
    vp <- pca$sdev^2 / sum(pca$sdev^2) * 100
    d <- as.data.frame(pca$x[,1:3,drop=FALSE]); names(d) <- c("PC1","PC2","PC3")
    d$Profile <- if (!is.null(res()$km) && length(res()$km$cluster)==nrow(d))
      factor(res()$km$cluster) else factor("1")
    d$Hover <- paste0(
      "Exploratory Technical Profile: ",d$Profile,
      "<br>PC1: ",round(d$PC1,3),"<br>PC2: ",round(d$PC2,3),"<br>PC3: ",round(d$PC3,3))
    plotly::plot_ly(d,x=~PC1,y=~PC2,z=~PC3,color=~Profile,
      colors=unname(pal_cluster),type="scatter3d",mode="markers",
      text=~Hover,hoverinfo="text",
      marker=list(size=4,opacity=.72)) %>%
      plotly::layout(
        title=sprintf("Interactive PCA 3D — PC1 + PC2 + PC3 = %.2f%% cumulative variance",sum(vp[1:3])),
        scene=list(
          xaxis=list(title=sprintf("PC1 (%.2f%%)",vp[1])),
          yaxis=list(title=sprintf("PC2 (%.2f%%)",vp[2])),
          zaxis=list(title=sprintf("PC3 (%.2f%%)",vp[3]))
        ),
        legend=list(title=list(text="Exploratory Technical Profile"))
      )
  }))

  output$acp_3d_note <- renderUI({
    req(res())
    pca <- res()$pca_res
    if (is.null(pca) || length(pca$sdev)<3) return(NULL)
    vp <- pca$sdev^2/sum(pca$sdev^2)*100
    tags$div(class="nota-metodo",
      sprintf("PC1, PC2 and PC3 explain %.2f%% of cumulative variance. Rotate, zoom and pan freely; click legend entries to show/hide exploratory technical profiles.",sum(vp[1:3])))
  })

  tech_choices <- reactive({
    req(res())
    cols <- intersect(res()$vars_cols,names(res()$base))
    stats::setNames(cols, unname(TECH_LABELS[cols]))
  })
  output$cluster_x_ui <- renderUI({
    ch <- tech_choices(); selectInput("cluster_x","X",choices=ch,selected=unname(ch)[1])
  })
  output$cluster_y_ui <- renderUI({
    ch <- tech_choices(); selectInput("cluster_y","Y",choices=ch,selected=unname(ch)[min(2,length(ch))])
  })
  output$cluster_z_ui <- renderUI({
    ch <- tech_choices(); selectInput("cluster_z","Z",choices=ch,selected=unname(ch)[min(3,length(ch))])
  })
  output$cluster_profile_ui <- renderUI({
    req(res())
    lev <- levels(droplevels(res()$base$cluster))
    checkboxGroupInput("cluster_profiles","Profiles",choices=lev,selected=lev)
  })

  output$cluster_3d <- plotly::renderPlotly(tr_plotly({
    req(res(),input$cluster_x,input$cluster_y,input$cluster_z)
    validate(need(length(unique(c(input$cluster_x,input$cluster_y,input$cluster_z)))==3,
                  "Choose three different technical variables for X, Y and Z."))
    d <- res()$base %>%
      dplyr::filter(!is.na(cluster)) %>%
      dplyr::mutate(Profile=factor(cluster))
    if (!is.null(input$cluster_profiles))
      d <- d %>% dplyr::filter(as.character(Profile) %in% input$cluster_profiles)
    validate(need(nrow(d)>0,"No observations remain after filtering."))
    d$Hover <- paste0(
      "Exploratory Technical Profile: ",d$Profile,
      "<br>",TECH_LABELS[input$cluster_x],": ",round(d[[input$cluster_x]],3),
      "<br>",TECH_LABELS[input$cluster_y],": ",round(d[[input$cluster_y]],3),
      "<br>",TECH_LABELS[input$cluster_z],": ",round(d[[input$cluster_z]],3))
    plotly::plot_ly(d,
      x=d[[input$cluster_x]],y=d[[input$cluster_y]],z=d[[input$cluster_z]],
      color=~Profile,colors=unname(pal_cluster),type="scatter3d",mode="markers",
      text=~Hover,hoverinfo="text",
      marker=list(size=ifelse(is.null(input$cluster_point_size),5,input$cluster_point_size),
                  opacity=ifelse(is.null(input$cluster_alpha),.7,input$cluster_alpha))) %>%
      plotly::layout(
        title="K-Means 3D — Exploratory Technical Profiles",
        scene=list(
          xaxis=list(title=TECH_LABELS[input$cluster_x]),
          yaxis=list(title=TECH_LABELS[input$cluster_y]),
          zaxis=list(title=TECH_LABELS[input$cluster_z])
        ),
        legend=list(title=list(text="Exploratory Technical Profile"))
      )
  }))

  output$acp_eig <- renderPlot({
    req(res())
    pca <- res()$pca_res
    if (is.null(pca)) { plot_msg("PCA no disponible\n(datos con varianza cero o insuficientes)"); return(invisible(NULL)) }
    tryCatch({
      n_comp <- min(length(pca$sdev), 10)
      d <- data.frame(
        PC       = factor(paste0("PC",1:n_comp), levels=paste0("PC",1:n_comp)),
        varianza = round(pca$sdev[1:n_comp]^2 / sum(pca$sdev^2) * 100, 1)
      )
      d$acum <- cumsum(d$varianza)
      p <- ggplot2::ggplot(d, ggplot2::aes(PC, varianza)) +
        ggplot2::geom_col(fill="#AED6F1", color="white", width=0.7) +
        ggplot2::geom_line(ggplot2::aes(y=acum, group=1),
                           color="#F1948A", linewidth=1.3) +
        ggplot2::geom_point(ggplot2::aes(y=acum), color="#F1948A", size=3.5) +
        ggplot2::geom_text(ggplot2::aes(label=paste0(varianza,"%")),
                           vjust=-0.6, size=3.5, fontface="bold", color="#4A4A4A") +
        ggplot2::geom_hline(yintercept=80, linetype="dashed",
                            color="gray50", linewidth=0.8) +
        ggplot2::annotate("text", x=n_comp, y=82,
                          label="Umbral 80%", size=3.2,
                          color="gray50", hjust=1) +
        ggplot2::scale_y_continuous(limits=c(0,110)) +
        ggplot2::labs(
          title="Varianza explicada por componente principal (ACP)",
          subtitle="Barras azules = varianza individual | Línea rosada = varianza acumulada | Umbral recomendado 80%",
          x="Componente Principal", y="Varianza explicada (%)",
          caption="Calculado solo con las variables técnicas del dataset de incidentes") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"),
                       axis.text.x=ggplot2::element_text(angle=45, hjust=1))
      guardar_plot(p, "acp_eig"); p
    }, error=function(e) { plot_msg(paste("Error ACP varianza:\n", e$message), "red") })
  })

  output$acp_biplot <- renderPlot({
    req(res())
    pca <- res()$pca_res
    if (is.null(pca) || ncol(pca$x) < 2) {
      plot_msg("Biplot no disponible\n(necesita >= 2 componentes)"); return(invisible(NULL))
    }
    tryCatch({
      scores <- as.data.frame(pca$x[, 1:2])
      names(scores) <- c("PC1","PC2")
      var_pct <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)

      km <- res()$km
      if (!is.null(km) && length(km$cluster) == nrow(scores)) {
        scores$Cluster <- factor(km$cluster)
      } else {
        scores$Cluster <- factor("1")
      }

      rot <- as.data.frame(pca$rotation[,1:2])
      names(rot) <- c("PC1","PC2")
      sf <- max(abs(scores[,1:2])) / max(abs(rot)) * 0.65
      rot <- rot * sf
      rot$variable <- rownames(pca$rotation)

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(data=scores,
                            ggplot2::aes(PC1, PC2, color=Cluster),
                            alpha=0.6, size=1.8) +
        ggplot2::scale_color_manual(values=pal_cluster, name="Cluster") +
        ggplot2::geom_segment(data=rot,
                              ggplot2::aes(x=0,y=0,xend=PC1,yend=PC2),
                              arrow=ggplot2::arrow(length=ggplot2::unit(.28,"cm")),
                              color="#E59896", linewidth=0.9) +
        ggplot2::geom_text(data=rot,
                           ggplot2::aes(PC1*1.13, PC2*1.13, label=variable),
                           color="#C0392B", size=3.3, fontface="bold") +
        ggplot2::geom_hline(yintercept=0, linetype="dashed", color="gray75") +
        ggplot2::geom_vline(xintercept=0, linetype="dashed", color="gray75") +
        ggplot2::labs(
          title="Biplot ACP — Individuos y variables técnicas (PC1 vs PC2)",
          subtitle="Puntos = observaciones coloreadas por cluster | Flechas = variables originales",
          x=paste0("PC1 (",var_pct[1],"% varianza explicada)"),
          y=paste0("PC2 (",var_pct[2],"% varianza explicada)"),
          caption="Flechas largas = mayor contribución al componente | Flechas paralelas = variables correlacionadas") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"))
      guardar_plot(p, "acp_biplot"); p
    }, error=function(e) { plot_msg(paste("Error Biplot:\n", e$message), "red") })
  })

  output$acp_tbl <- renderDT({
    req(res())
    pca <- res()$pca_res
    if (is.null(pca)) {
      return(datatable(data.frame(
        Mensaje="PCA no disponible. Verifique que el dataset tenga varianza en todas las variables.")))
    }
    var_pct <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 2)
    data.frame(
      Componente    = paste0("PC", seq_along(pca$sdev)),
      Valor_Propio  = round(pca$sdev^2, 4),
      Varianza_pct  = var_pct,
      Acumulada_pct = round(cumsum(var_pct), 2)
    ) %>% datatable(caption="Valores propios y varianza explicada (solo variables técnicas)")
  })

  # ── CLUSTER ───────────────────────────────────────────────
  output$cluster_plot <- renderPlot({
    req(res())
    km  <- res()$km
    pca <- res()$pca_res
    if (is.null(km)) {
      plot_msg(paste0("K-Means no disponible\n(datos insuficientes para k = ",
                      res()$k_selected, ")"))
      return(invisible(NULL))
    }
    if (is.null(pca) || ncol(pca$x) < 2) {
      plot_msg("PCA no disponible para visualizar clusters"); return(invisible(NULL))
    }
    tryCatch({
      scores <- as.data.frame(pca$x[, 1:2])
      names(scores) <- c("PC1","PC2")
      var_pct <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)

      n_km <- length(km$cluster)
      n_sc <- nrow(scores)
      if (n_km == n_sc) {
        scores$Cluster <- factor(km$cluster)
      } else {
        scores$Cluster <- factor(rep(seq_len(res()$k_selected), length.out=n_sc))
      }

      centros <- scores %>%
        dplyr::group_by(Cluster) %>%
        dplyr::summarise(PC1=mean(PC1), PC2=mean(PC2), .groups="drop")

      p <- ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, color=Cluster, fill=Cluster)) +
        ggplot2::stat_ellipse(geom="polygon", alpha=0.12, linewidth=0.8, level=0.90) +
        ggplot2::geom_point(alpha=0.6, size=2) +
        ggplot2::geom_point(data=centros,
                            ggplot2::aes(PC1,PC2,color=Cluster),
                            shape=8, size=5, stroke=2, show.legend=FALSE) +
        ggplot2::geom_text(data=centros,
                           ggplot2::aes(PC1, PC2, label=paste("C",Cluster)),
                           vjust=-1.2, size=4, fontface="bold", show.legend=FALSE) +
        ggplot2::scale_color_manual(values=pal_cluster, name="Cluster") +
        ggplot2::scale_fill_manual(values=pal_cluster,  name="Cluster") +
        ggplot2::geom_hline(yintercept=0, linetype="dashed", color="gray75") +
        ggplot2::geom_vline(xintercept=0, linetype="dashed", color="gray75") +
        ggplot2::labs(
          title=paste0("Segmentación K-Means (k = ", res()$k_selected,
                       ") sobre los 2 primeros componentes principales"),
          subtitle="Puntos = grupos año×país×industria×ataque | ✱ = centroide | Elipse = 90% de los datos",
          x=paste0("PC1 (", var_pct[1], "% varianza)"),
          y=paste0("PC2 (", var_pct[2], "% varianza)"),
          caption="Basado en: frecuencia, impacto, severidad, tiempo de respuesta, duración, tasa de éxito y resiliencia") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"),
                       legend.position="right")
      guardar_plot(p,"cluster"); p
    }, error=function(e) { plot_msg(paste("Error Cluster:\n", e$message), "red") })
  })

  output$cluster_vb_sil_selected <- renderValueBox({
    req(res())
    v <- res()$k_selected_sil
    valueBox(
      ifelse(is.finite(v), sprintf("%.3f", v), "NA"),
      paste0("Silhouette medio para k = ", res()$k_selected),
      icon=icon("chart-simple"), color="aqua"
    )
  })

  output$cluster_vb_sil_max <- renderValueBox({
    req(res())
    kmax <- res()$k_sil_max
    vmax <- res()$k_sil_max_value
    valueBox(
      ifelse(is.finite(vmax), sprintf("%.3f", vmax), "NA"),
      ifelse(is.finite(kmax),
             paste0("Máximo empírico de Silhouette — k = ", kmax),
             "Máximo empírico no disponible"),
      icon=icon("arrow-up-right-dots"), color="green"
    )
  })

  output$cluster_vb_k <- renderValueBox({
    req(res())
    valueBox(
      paste0("k = ", res()$k_selected),
      "Solución final retenida: codo + Silhouette + interpretabilidad",
      icon=icon("object-group"), color="purple"
    )
  })

  output$cluster_tbl <- renderDT({
    req(res())
    z <- res()$cluster_profiles

    if (isTRUE(res()$tiene_encuesta) && !is.null(res()$scores_sector)) {
      x <- res()$base %>%
        dplyr::filter(!is.na(cluster)) %>%
        dplyr::left_join(
          res()$scores_sector %>% dplyr::select(industria, EIglobal),
          by="industria")

      org <- x %>%
        dplyr::group_by(cluster) %>%
        dplyr::summarise(
          EIglobal_external=ifelse(
            all(is.na(EIglobal)), NA_real_, mean(EIglobal,na.rm=TRUE)),
          .groups="drop"
        )
      z <- z %>% dplyr::left_join(org, by="cluster")
    }

    datatable(
      z,
      caption=if (isTRUE(res()$tiene_encuesta))
        paste0(
          "Technical cluster characterization. EIglobal is an external contrast only; ",
          "it did not participate in clustering.")
      else
        "Technical cluster characterization. Optional Organizational Module: Not available.",
      options=list(scrollX=TRUE, pageLength=10),
      rownames=FALSE
    )
  })

  output$cluster_kdiag_tbl <- renderDT({
    req(res())
    d <- res()$k_diag %>%
      dplyr::mutate(
        selected = ifelse(k == res()$k_selected, "Final retained solution", ""),
        WSS = round(WSS, 3),
        silhouette_mean = round(silhouette_mean, 4)
      )
    datatable(
      d,
      caption=paste0(
        K_FINAL_RATIONALE,
        " The empirical Silhouette maximum is reported independently."),
      options=list(dom="tip", pageLength=10),
      rownames=FALSE
    )
  })

  # ── MODELO ────────────────────────────────────────────────
  output$mdl_tipo <- renderPrint({
    req(res())
    cat("=== Modelo estadístico aplicado ===\n")
    cat("Tipo        :", res()$tipo_mdl, "\n")
    d <- res()$dispersion_diag
    cat("Dispersión  :", round(d$dispersion_ratio,3), "—", d$status, "\n")
    cat("p_under     :", signif(d$p_under,4),
        "| p_over:", signif(d$p_over,4), "\n")
    cat("Criterio    : pruebas direccionales basadas en las colas inferior y superior de X² de Pearson, con α = 0.05.\n")
    if (identical(res()$tipo_mdl, "Quasi-Poisson"))
      cat("AIC/BIC     : no definidos para modelos de cuasi-verosimilitud.\n")
    cat("\nVariable dependiente : frecuencia de incidentes (n_incidentes)\n")
    cat("Variables independientes (solo dataset técnico):\n")
    cat("  · anio, industria, tipo_ataque    (predictores contextuales)\n")
    cat("  · log_impacto, severidad_media    (magnitud del ataque)\n")
    cat("  · tasa_exito, idx_resiliencia     (desempeño defensivo observado)\n")
    cat("\nLas dimensiones del cuestionario (FI, SI, IE, RD, PG, EIglobal)\n")
    cat("NO forman parte del modelo: se reportan como Optional\n")
    cat("Organizational Module.\n")
  })

  output$mdl_res <- renderPrint({
    req(res())
    if (is.null(res()$mdl_final)) {
      cat("El modelo no pudo ajustarse con los datos actuales.\n")
      cat("Revise la pestaña Technical Core → 4. Minería para detalles.\n")
    } else {
      summary(res()$mdl_final)
    }
  })

  output$mdl_coef <- renderDT({
    req(res())
    if (nrow(res()$tbl_coef) == 0) {
      return(datatable(data.frame(Info="Coeficientes no disponibles")))
    }
    res()$tbl_coef %>%
      dplyr::mutate(
        significancia = dplyr::case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.1   ~ ".",
          TRUE            ~ ""
        ),
        across(where(is.numeric),~round(.,4))
      ) %>%
      datatable(
        caption="IRR = Incidence Rate Ratio. IRR>1 aumenta frecuencia; IRR<1 la reduce",
        options=list(scrollX=TRUE))
  })

  output$mdl_metricas <- renderDT({
    req(res())
    r2 <- tryCatch(
      cor(res()$base_res$n_incidentes, res()$base_res$ajustados,
          use="complete.obs")^2, error=function(e) NA)
    res()$resumen_mdl %>%
      dplyr::mutate(r2_pearson_obs_ajust = r2) %>%
      dplyr::mutate(across(where(is.numeric),~round(.,3))) %>%
      datatable(caption=paste0("Métricas globales. r2_pearson_obs_ajust = ",
        "cor(observado, ajustado)² dentro de muestra; no es el R² de un modelo lineal"),
        options=list(scrollX=TRUE))
  })

  # ── DIAGNÓSTICO DEL AJUSTE ────────────────────────────────
  output$pred_scatter <- renderPlot({
    req(res())
    d <- res()$base_res
    r2 <- tryCatch({
      cor(d$n_incidentes, d$ajustados, use="complete.obs")^2
    }, error=function(e) NA)
    p <- ggplot2::ggplot(d, ggplot2::aes(n_incidentes, ajustados)) +
      ggplot2::geom_point(alpha=0.5, color="#85C1E9", size=2) +
      ggplot2::geom_abline(linetype="dashed", color="#F1948A", linewidth=1) +
      ggplot2::geom_smooth(method="lm", se=TRUE, color="#5D6D7E",
                           fill="#D5DBDB", alpha=0.3) +
      ggplot2::labs(
        title="Incidentes observados vs. ajustados por el modelo",
        subtitle=if(!is.na(r2)) paste0("r² de Pearson = ",round(r2,3),
          " — línea rosada punteada = ajuste perfecto") else "Línea rosada = ajuste perfecto",
        x="Frecuencia observada de incidentes",
        y="Frecuencia ajustada por el modelo",
        caption="r² = cor(observado, ajustado)²; ajuste dentro de muestra, no validación predictiva") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"pred_scatter"); p
  })

  output$pred_riesgo <- renderPlot({
    req(res())
    d <- res()$base_res %>%
      dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(
        pct=round(n/sum(n)*100,1),
        label=paste0(n,"\n(",pct,"%)")
      )
    p <- ggplot2::ggplot(d, ggplot2::aes(cat_frec_ajustada, n, fill=cat_frec_ajustada)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.6) +
      ggplot2::geom_text(ggplot2::aes(label=label),
                         vjust=-0.3, size=4, fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_riesgo) +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.2))) +
      ggplot2::labs(
        title="Grupos por tercil de frecuencia ajustada",
        subtitle="Cada grupo = combinación año × país × industria × tipo de ataque",
        x="Tercil de frecuencia ajustada", y="Número de grupos",
        caption=CAT_FREC_TXT) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"pred_cat_frec"); p
  })

  output$pred_tbl <- renderDT({
    req(res())
    res()$base_res %>%
      dplyr::select(anio, mes, pais, industria, tipo_ataque,
             n_incidentes, ajustados, error_pred, cat_frec_ajustada) %>%
      dplyr::rename(`cat_frec_ajustada (tercil)` = cat_frec_ajustada) %>%
      dplyr::mutate(across(where(is.numeric),~round(.,2))) %>%
      datatable(
        caption=paste0("Valores ajustados por grupo. Error = observado − ajustado. ",
                       CAT_FREC_TXT),
        options=list(scrollX=TRUE, pageLength=15))
  })

  # ── PROYECCIÓN A FUTURO (selector de año) ─────────────────
  output$pred_anio_ui <- renderUI({
    req(res())
    anios  <- sort(unique(res()$base$anio))
    a_max  <- max(anios, na.rm=TRUE)
    tagList(
      selectInput("pred_anio_base",
        "Año base (referencia de patrones):",
        choices=anios, selected=a_max),
      selectInput("pred_anio_obj",
        "Año a proyectar:",
        choices=seq(a_max+1, a_max+5), selected=a_max+1)
    )
  })

  # [v7.3.2] Proyección con comparaciones internamente consistentes.
  # Los KPI temporales comparan predicción vs predicción usando la MISMA
  # estructura de grupos del año base. Esto evita mezclar un total observado
  # con un total modelado al calcular la variación esperada.
  #
  # Los terciles se fijan UNA SOLA VEZ con la distribución predicha del año
  # base y esos mismos puntos de corte se aplican al año proyectado. Por ello,
  # el porcentaje del tercil superior ya no queda mecánicamente cerca de 33%.
  pred_futuro <- reactive({
    req(res(), input$pred_anio_obj, input$pred_anio_base)
    r <- res()
    if (is.null(r$mdl_final)) return(NULL)

    a_base <- as.numeric(input$pred_anio_base)
    a_obj  <- as.numeric(input$pred_anio_obj)

    plantilla <- r$base %>% dplyr::filter(anio == a_base)
    if (nrow(plantilla) == 0) {
      a_base_real <- max(r$base$anio, na.rm=TRUE)
      plantilla <- r$base %>% dplyr::filter(anio == a_base_real)
      a_base <- a_base_real
    }

    base_pred <- plantilla %>% dplyr::mutate(anio = a_base)
    futuro    <- plantilla %>% dplyr::mutate(anio = a_obj)

    pred_base <- tryCatch(
      predict(r$mdl_final, newdata=base_pred, type="response"),
      error=function(e) NULL
    )
    pred_fut <- tryCatch(
      predict(r$mdl_final, newdata=futuro, type="response"),
      error=function(e) NULL
    )
    if (is.null(pred_base) || is.null(pred_fut)) return(NULL)

    base_pred$pred_incidentes <- as.numeric(pred_base)
    futuro$pred_incidentes    <- as.numeric(pred_fut)

    # Puntos de corte FIJOS calculados sobre la predicción del año base.
    q_base <- stats::quantile(
      base_pred$pred_incidentes,
      probs=c(0, .33, .66, 1),
      na.rm=TRUE,
      names=FALSE
    )
    q_base <- as.numeric(q_base)

    # `cut()` exige puntos estrictamente crecientes. Si hay empates,
    # se usa ranking percentil como fallback, manteniendo la referencia base.
    cortes_validos <- length(q_base) == 4 &&
      all(is.finite(q_base)) &&
      all(diff(q_base) > 0)

    if (cortes_validos) {
      base_pred$cat_frec_ref <- cut(
        base_pred$pred_incidentes,
        breaks=q_base,
        labels=c("Bajo","Medio","Alto"),
        include.lowest=TRUE
      )
      futuro$cat_frec_proy <- cut(
        futuro$pred_incidentes,
        breaks=c(-Inf, q_base[2], q_base[3], Inf),
        labels=c("Bajo","Medio","Alto"),
        include.lowest=TRUE
      )
    } else {
      # Fallback reproducible cuando la distribución base contiene demasiados empates.
      rnk <- rank(base_pred$pred_incidentes, ties.method="average", na.last="keep")
      pct <- rnk / max(rnk, na.rm=TRUE)
      base_pred$cat_frec_ref <- cut(
        pct, breaks=c(-Inf,.33,.66,Inf),
        labels=c("Bajo","Medio","Alto"), include.lowest=TRUE
      )

      # Umbrales numéricos derivados de los grupos base resultantes.
      low_thr  <- suppressWarnings(max(
        base_pred$pred_incidentes[base_pred$cat_frec_ref=="Bajo"], na.rm=TRUE))
      high_thr <- suppressWarnings(max(
        base_pred$pred_incidentes[base_pred$cat_frec_ref %in% c("Bajo","Medio")],
        na.rm=TRUE))

      if (!is.finite(low_thr))  low_thr  <- stats::quantile(
        base_pred$pred_incidentes, .33, na.rm=TRUE, names=FALSE)
      if (!is.finite(high_thr)) high_thr <- stats::quantile(
        base_pred$pred_incidentes, .66, na.rm=TRUE, names=FALSE)

      futuro$cat_frec_proy <- cut(
        futuro$pred_incidentes,
        breaks=c(-Inf, low_thr, high_thr, Inf),
        labels=c("Bajo","Medio","Alto"),
        include.lowest=TRUE
      )
      q_base <- c(min(base_pred$pred_incidentes, na.rm=TRUE),
                  low_thr, high_thr,
                  max(base_pred$pred_incidentes, na.rm=TRUE))
    }

    base_pred$cat_frec_ref <- factor(
      base_pred$cat_frec_ref, levels=c("Bajo","Medio","Alto"))
    futuro$cat_frec_proy <- factor(
      futuro$cat_frec_proy, levels=c("Bajo","Medio","Alto"))

    # Cambio de categoría usando exactamente las mismas filas/grupos.
    futuro$cat_base_ref <- base_pred$cat_frec_ref
    futuro$entra_alto <- futuro$cat_base_ref != "Alto" &
                         futuro$cat_frec_proy == "Alto"
    futuro$sale_alto  <- futuro$cat_base_ref == "Alto" &
                         futuro$cat_frec_proy != "Alto"

    hist_anual <- r$base %>%
      dplyr::group_by(anio) %>%
      dplyr::summarise(total=sum(n_incidentes), .groups="drop") %>%
      dplyr::mutate(tipo="Histórico")

    total_proy     <- sum(futuro$pred_incidentes, na.rm=TRUE)
    base_pred_total<- sum(base_pred$pred_incidentes, na.rm=TRUE)
    base_obs_total <- sum(plantilla$n_incidentes, na.rm=TRUE)

    var_pct <- if (is.finite(base_pred_total) && base_pred_total > 0)
      (total_proy - base_pred_total) / base_pred_total * 100 else NA_real_

    # Resúmenes comparables para los KPI de ataque e industria.
    atk_base <- base_pred %>%
      dplyr::group_by(tipo_ataque) %>%
      dplyr::summarise(base=sum(pred_incidentes, na.rm=TRUE), .groups="drop")
    atk_fut <- futuro %>%
      dplyr::group_by(tipo_ataque) %>%
      dplyr::summarise(proy=sum(pred_incidentes, na.rm=TRUE), .groups="drop")
    atk_cmp <- dplyr::left_join(atk_fut, atk_base, by="tipo_ataque") %>%
      dplyr::mutate(
        cambio_pct=dplyr::if_else(
          is.finite(base) & base > 0,
          (proy-base)/base*100,
          NA_real_
        )
      ) %>%
      dplyr::arrange(dplyr::desc(proy))

    ind_base <- base_pred %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(base=sum(pred_incidentes, na.rm=TRUE), .groups="drop")
    ind_fut <- futuro %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(proy=sum(pred_incidentes, na.rm=TRUE), .groups="drop")
    ind_cmp <- dplyr::left_join(ind_fut, ind_base, by="industria") %>%
      dplyr::mutate(
        cambio_pct=dplyr::if_else(
          is.finite(base) & base > 0,
          (proy-base)/base*100,
          NA_real_
        )
      ) %>%
      dplyr::arrange(dplyr::desc(proy))

    n_entra <- sum(futuro$entra_alto, na.rm=TRUE)
    n_sale  <- sum(futuro$sale_alto, na.rm=TRUE)
    n_alto  <- sum(futuro$cat_frec_proy=="Alto", na.rm=TRUE)
    pct_alto<- if (nrow(futuro)>0) n_alto/nrow(futuro)*100 else NA_real_

    list(
      futuro=futuro,
      base_pred=base_pred,
      hist_anual=hist_anual,
      a_base=a_base,
      a_obj=a_obj,
      total_proy=total_proy,
      base_pred_total=base_pred_total,
      base_obs_total=base_obs_total,
      var_pct=var_pct,
      q_base=q_base,
      n_entra=n_entra,
      n_sale=n_sale,
      n_alto=n_alto,
      pct_alto=pct_alto,
      atk_cmp=atk_cmp,
      ind_cmp=ind_cmp
    )
  })

  output$fut_vb_total <- renderValueBox({
    pf <- pred_futuro()
    if (is.null(pf)) return(valueBox(
      "—","Proyección no disponible", icon=icon("ban"), color="yellow"))
    valueBox(
      scales::comma(round(pf$total_proy,0)),
      paste0("Incidentes proyectados para ", pf$a_obj),
      icon=icon("chart-line"), color="blue"
    )
  })

  output$fut_vb_var <- renderValueBox({
    pf <- pred_futuro(); req(pf)
    if (!is.finite(pf$var_pct)) {
      return(valueBox("—","Variación proyectada no disponible",
                      icon=icon("minus"), color="yellow"))
    }
    signo <- ifelse(pf$var_pct >= 0, "+", "")
    valueBox(
      paste0(signo, round(pf$var_pct,1), "%"),
      paste0("Variación proyectada vs predicción base ", pf$a_base),
      icon=icon(ifelse(pf$var_pct >= 0,"arrow-trend-up","arrow-trend-down")),
      color=ifelse(pf$var_pct >= 0, "red", "green")
    )
  })

  output$fut_vb_alto <- renderValueBox({
    pf <- pred_futuro(); req(pf)

    # Ya NO muestra ~33% por construcción. Cuenta únicamente grupos que
    # cambian desde Bajo/Medio en el año base a Alto con cortes base fijos.
    if (pf$n_entra > 0) {
      val <- paste0("+", pf$n_entra)
      sub <- paste0(
        "Grupos que entran al tercil superior vs ", pf$a_base,
        " | Alto proyectado: ", round(pf$pct_alto,1), "%"
      )
      col <- "red"
      ico <- "arrow-up"
    } else if (pf$n_sale > 0) {
      val <- paste0("-", pf$n_sale)
      sub <- paste0(
        "Grupos que salen del tercil superior vs ", pf$a_base,
        " | Alto proyectado: ", round(pf$pct_alto,1), "%"
      )
      col <- "green"
      ico <- "arrow-down"
    } else {
      val <- "0"
      sub <- paste0(
        "Sin cambios de pertenencia al tercil superior vs ", pf$a_base,
        " | Alto proyectado: ", round(pf$pct_alto,1), "%"
      )
      col <- "aqua"
      ico <- "equals"
    }

    valueBox(val, sub, icon=icon(ico), color=col)
  })

  output$fut_vb_ataque <- renderValueBox({
    pf <- pred_futuro(); req(pf)
    top <- pf$atk_cmp[1,,drop=FALSE]
    if (nrow(top)==0) return(valueBox(
      "—","Ataque dominante no disponible", icon=icon("bug"), color="yellow"))

    cambio <- if (is.finite(top$cambio_pct[1]))
      paste0(ifelse(top$cambio_pct[1]>=0,"+",""),
             round(top$cambio_pct[1],1), "%") else "NA"

    valueBox(
      as.character(top$tipo_ataque[1]),
      paste0(
        "Dominante en ", pf$a_obj, ": ",
        scales::comma(round(top$proy[1],0)),
        " incidentes | ", cambio, " vs ", pf$a_base
      ),
      icon=icon("bug"), color="purple"
    )
  })

  output$fut_vb_ind <- renderValueBox({
    pf <- pred_futuro(); req(pf)
    top <- pf$ind_cmp[1,,drop=FALSE]
    if (nrow(top)==0) return(valueBox(
      "—","Industria dominante no disponible",
      icon=icon("building"), color="yellow"))

    cambio <- if (is.finite(top$cambio_pct[1]))
      paste0(ifelse(top$cambio_pct[1]>=0,"+",""),
             round(top$cambio_pct[1],1), "%") else "NA"

    valueBox(
      as.character(top$industria[1]),
      paste0(
        "Mayor frecuencia en ", pf$a_obj, ": ",
        scales::comma(round(top$proy[1],0)),
        " incidentes | ", cambio, " vs ", pf$a_base
      ),
      icon=icon("building"), color="orange"
    )
  })

  output$fut_trend <- renderPlot({
    pf <- pred_futuro()
    if (is.null(pf)) { plot_msg("Proyección no disponible\n(modelo no ajustado)"); return(invisible(NULL)) }
    d_h <- pf$hist_anual
    d_p <- data.frame(anio=pf$a_obj, total=pf$total_proy, tipo="Proyectado")
    d   <- dplyr::bind_rows(d_h, d_p)
    var_txt <- if (is.finite(pf$var_pct))
      paste0(ifelse(pf$var_pct>=0,"+",""), round(pf$var_pct,1), "%") else "no disponible"
    p <- ggplot2::ggplot(d, ggplot2::aes(anio, total)) +
      ggplot2::geom_area(data=d_h, fill="#D6EAF8", alpha=0.6) +
      ggplot2::geom_line(data=d_h, color="#7FB3D5", linewidth=1.4) +
      ggplot2::geom_point(data=d_h, color="#7FB3D5", size=3.5) +
      ggplot2::geom_segment(x=max(d_h$anio), xend=pf$a_obj,
                            y=d_h$total[which.max(d_h$anio)], yend=pf$total_proy,
                            linetype="dashed", color="#F1948A", linewidth=1.1) +
      ggplot2::geom_point(data=d_p, color="#F1948A", size=6, shape=18) +
      ggplot2::geom_text(data=d_p,
                         ggplot2::aes(label=paste0(pf$a_obj,"\n",
                                                    scales::comma(round(total,0)))),
                         vjust=-0.7, size=3.8, fontface="bold", color="#C0392B") +
      ggplot2::geom_text(data=d_h,
                         ggplot2::aes(label=scales::comma(total)),
                         vjust=-1, size=3.2, color="#4A4A4A") +
      ggplot2::scale_x_continuous(breaks=c(d_h$anio, pf$a_obj)) +
      ggplot2::scale_y_continuous(labels=scales::comma,
                                  expand=ggplot2::expansion(mult=c(0,.22))) +
      ggplot2::labs(
        title=paste0("Incidentes anuales: histórico y proyección a ", pf$a_obj),
        subtitle=paste0("Rombo rosado = proyección del modelo GLM (",
                        res()$tipo_mdl, ") | Variación proyectada: ", var_txt,
                        " vs predicción base ", pf$a_base),
        x="Año", y="Total de incidentes",
        caption="La proyección asume la estructura de grupos (país × industria × ataque) del año base") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"fut_trend"); p
  })

  output$fut_tipo <- renderPlot({
    pf <- pred_futuro()
    if (is.null(pf)) { plot_msg("Proyección no disponible"); return(invisible(NULL)) }
    d <- pf$futuro %>%
      dplyr::group_by(tipo_ataque) %>%
      dplyr::summarise(p=sum(pred_incidentes), .groups="drop") %>%
      dplyr::mutate(pct=round(p/sum(p)*100,1),
                    lbl=paste0(scales::comma(round(p,0))," (",pct,"%)"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(tipo_ataque,p), p, fill=tipo_ataque)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.7) +
      ggplot2::geom_text(ggplot2::aes(label=lbl), hjust=-0.05, size=3.4,
                         fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque) +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.32))) +
      ggplot2::labs(
        title=paste0("Incidentes proyectados por tipo de ataque — ", pf$a_obj),
        subtitle="Prioridades de defensa para el año seleccionado",
        x=NULL, y="Incidentes proyectados",
        caption="Suma de predicciones del modelo sobre todos los grupos país × industria") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"fut_tipo"); p
  })

  output$fut_ind <- renderPlot({
    pf <- pred_futuro()
    if (is.null(pf)) { plot_msg("Proyección no disponible"); return(invisible(NULL)) }
    d <- pf$futuro %>%
      dplyr::group_by(industria, cat_frec_proy) %>%
      dplyr::summarise(p=sum(pred_incidentes), .groups="drop")
    ord <- d %>% dplyr::group_by(industria) %>%
      dplyr::summarise(t=sum(p), .groups="drop") %>%
      dplyr::arrange(t)
    d$industria <- factor(d$industria, levels=ord$industria)
    p <- ggplot2::ggplot(d, ggplot2::aes(industria, p, fill=cat_frec_proy)) +
      ggplot2::geom_col(position="stack", width=0.7) +
      ggplot2::scale_fill_manual(values=pal_riesgo, name="Tercil frec.\nproyectada") +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels=scales::comma) +
      ggplot2::labs(
        title=paste0("Frecuencia proyectada por industria — ", pf$a_obj),
        subtitle="Barras apiladas por tercil de frecuencia proyectada de cada grupo",
        x=NULL, y="Incidentes proyectados",
        caption=CAT_FREC_PROY_TXT) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"fut_ind"); p
  })

  output$fut_recs <- renderUI({
    pf <- pred_futuro()
    if (is.null(pf)) return(tags$p("Cargue los datos para generar recomendaciones."))

    top_atq <- pf$futuro %>%
      dplyr::group_by(tipo_ataque) %>%
      dplyr::summarise(p=sum(pred_incidentes), .groups="drop") %>%
      dplyr::arrange(dplyr::desc(p)) %>% head(2)
    top_ind <- pf$futuro %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(p=sum(pred_incidentes), .groups="drop") %>%
      dplyr::arrange(dplyr::desc(p)) %>% head(2)
    grupos_alto <- pf$futuro %>%
      dplyr::filter(cat_frec_proy=="Alto") %>%
      dplyr::arrange(dplyr::desc(pred_incidentes)) %>% head(3)

    # Guarda contra var_pct no finito (evita error en el `if`)
    tendencia <- if (!is.finite(pf$var_pct)) "una variación no cuantificable"
      else if (pf$var_pct >= 10) "un aumento significativo"
      else if (pf$var_pct >= 0) "un aumento moderado"
      else if (pf$var_pct >= -10) "una reducción moderada"
      else "una reducción significativa"

    var_txt <- if (is.finite(pf$var_pct))
      paste0(ifelse(pf$var_pct>=0,"+",""), round(pf$var_pct,1), "%") else "NA"

    rec_item <- function(icono, colr, fondo, titulo, texto)
      div(style=paste0("border-left:5px solid ",colr,";background:",fondo,
                       ";padding:10px 14px;border-radius:6px;margin-bottom:10px;"),
        tags$b(icon(icono), " ", titulo),
        tags$p(HTML(texto), style="margin:4px 0 0 0;font-size:13px;line-height:1.55;"))

    tagList(
      div(style="background:#F4F9FB;padding:10px 14px;border-radius:8px;margin-bottom:12px;",
        tags$b(icon("chart-line"), paste0(" Escenario proyectado para ", pf$a_obj, ":")),
        tags$p(style="margin:4px 0 0 0;font-size:13px;",
          sprintf("El modelo anticipa %s de incidentes (%s respecto a la predicción base de %s), con un total estimado de %s casos.",
                  tendencia, var_txt, pf$a_base,
                  scales::comma(round(pf$total_proy,0))))),

      rec_item("triangle-exclamation", "#F1948A", "#FDF2F0",
        paste0("Prioridad 1 — Defensa anti-", top_atq$tipo_ataque[1]),
        paste0("Sería el vector dominante en ", pf$a_obj, " con ~",
               scales::comma(round(top_atq$p[1],0)),
               " incidentes proyectados. Asignar presupuesto prioritario, ",
               "actualizar playbooks y reforzar monitoreo específico de este vector.")),

      rec_item("building-shield", "#F0B27A", "#FDF6EE",
        paste0("Prioridad 2 — Sector ", top_ind$industria[1]),
        paste0("Industria con mayor frecuencia proyectada (~",
               scales::comma(round(top_ind$p[1],0)),
               " incidentes). Programar auditorías de seguridad y ejercicios de ",
               "respuesta a incidentes antes de iniciar el año ", pf$a_obj, ".")),

      rec_item("layer-group", "#C39BD3", "#F8F4FB",
        "Prioridad 3 — Grupos en el tercil superior proyectado",
        if (nrow(grupos_alto) > 0)
          paste0("Combinaciones con mayor frecuencia proyectada: ",
                 paste0(grupos_alto$pais, " · ", grupos_alto$industria, " · ",
                        grupos_alto$tipo_ataque, collapse=" | "),
                 ". Revisar controles y cobertura de monitoreo en estos frentes.")
        else "No se identificaron grupos en el tercil superior para el año proyectado."),

      rec_item("clipboard-check", "#7DCEA0", "#F0FAF4",
        "Planificación y seguimiento",
        paste0("Definir KPIs trimestrales para ", pf$a_obj,
               " (MTTR, tasa de éxito de ataques, cobertura de parcheo) y ",
               "reevaluar esta proyección al cierre de cada trimestre con datos reales.")),

      tags$p(style="font-size:11px;color:#95A5A6;margin-top:6px;",
        icon("circle-info"),
        paste0(" Proyección estadística basada en patrones históricos; supone estabilidad ",
               "estructural de los grupos país × industria × ataque. ", CAT_FREC_PROY_TXT))
    )
  })

  output$fut_tbl <- renderDT({
    pf <- pred_futuro(); req(pf)
    pf$futuro %>%
      dplyr::select(anio, pais, industria, tipo_ataque,
                    hist_incidentes=n_incidentes,
                    pred_incidentes, cat_frec_proy) %>%
      dplyr::rename(`cat_frec_proy (tercil)` = cat_frec_proy) %>%
      dplyr::mutate(pred_incidentes=round(pred_incidentes,1)) %>%
      dplyr::arrange(dplyr::desc(pred_incidentes)) %>%
      datatable(
        caption=paste0("Proyección ", pf$a_obj,
                       " por grupo (hist_incidentes = valor observado del año base ", pf$a_base, "). ",
                       CAT_FREC_PROY_TXT),
        options=list(scrollX=TRUE, pageLength=10), rownames=FALSE)
  })

  # ── DASHBOARD FINAL ───────────────────────────────────────
  output$dsh_vb_periodo <- renderValueBox({
    req(res())
    a <- range(res()$base$anio, na.rm=TRUE)
    valueBox(paste0(a[1]," – ",a[2]), "Período analizado",
             icon=icon("calendar"), color="aqua")
  })
  output$dsh_vb_inc <- renderValueBox({
    req(res())
    valueBox(scales::comma(nrow(res()$datos_limpios)),
             "Incidentes analizados", icon=icon("shield-halved"), color="blue")
  })
  output$dsh_vb_imp <- renderValueBox({
    req(res())
    valueBox(paste0(round(sum(res()$base$impacto_total)/1e6,1)," M"),
             "Impacto económico (escala)", icon=icon("dollar-sign"), color="red")
  })
  output$dsh_vb_modelo <- renderValueBox({
    req(res())
    valueBox(res()$tipo_mdl, "Modelo GLM aplicado",
             icon=icon("calculator"), color="purple")
  })
  output$dsh_vb_efx <- renderValueBox({
    req(res())
    if (isTRUE(res()$tiene_encuesta) && !is.null(res()$scores_enc)) {
      valueBox(paste0(round(res()$scores_enc$EIglobal,2)," / 5"),
               "EIglobal — optional organizational module",
               icon=icon("clipboard-check"), color="green")
    } else {
      valueBox("Not available",
               "Organizational questionnaire not provided — optional module",
               icon=icon("clipboard"), color="yellow")
    }
  })
  output$dsh_vb_exito <- renderValueBox({
    req(res())
    valueBox(scales::percent(res()$resumen_mdl$tasa_exito_global, accuracy=0.1),
             "Tasa de ataques exitosos", icon=icon("skull-crossbones"), color="orange")
  })
  output$dsh_vb_resp <- renderValueBox({
    req(res())
    valueBox(paste0(round(mean(res()$datos_limpios$t_respuesta_min,na.rm=TRUE),0)," min"),
             "Tiempo medio de respuesta (MTTR)", icon=icon("clock"), color="yellow")
  })
  output$dsh_vb_alto <- renderValueBox({
    req(res())
    d <- res()$base_res %>% dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(pct=round(n/sum(n)*100,1))
    pct_alto <- d$pct[d$cat_frec_ajustada=="Alto"]; if(!length(pct_alto)) pct_alto <- 0
    valueBox(paste0(pct_alto,"%"),
             "Grupos en el tercil superior de frecuencia ajustada",
             icon=icon("layer-group"), color="red")
  })

  output$dsh_trend <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>% dplyr::count(anio, tipo_ataque)
    tot <- res()$datos_limpios %>% dplyr::count(anio)
    p <- ggplot2::ggplot(d, ggplot2::aes(anio, n, fill=tipo_ataque)) +
      ggplot2::geom_area(alpha=0.85, color="white", linewidth=0.3) +
      ggplot2::geom_text(data=tot,
                         ggplot2::aes(anio, n, label=scales::comma(n), fill=NULL),
                         vjust=-0.5, size=3.4, fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque, name="Tipo de ataque") +
      ggplot2::scale_x_continuous(breaks=unique(d$anio)) +
      ggplot2::scale_y_continuous(labels=scales::comma,
                                  expand=ggplot2::expansion(mult=c(0,.15))) +
      ggplot2::labs(
        title="Evolución anual del volumen de incidentes",
        subtitle="Área apilada por tipo de ataque | Etiqueta = total anual",
        x="Año", y="Incidentes",
        caption="Fuente: dataset de incidentes procesado con metodología KDD") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(legend.position="bottom",
                     plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"dsh_trend"); p
  })

  output$dsh_donut <- renderPlot({
    req(res())
    d <- res()$base_res %>%
      dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(pct=round(n/sum(n)*100,1),
                    lbl=paste0(cat_frec_ajustada,"\n",pct,"%"),
                    ymax=cumsum(n), ymin=dplyr::lag(ymax, default=0),
                    ymid=(ymax+ymin)/2)
    p <- ggplot2::ggplot(d, ggplot2::aes(ymax=ymax,ymin=ymin,xmax=4,xmin=2.4,
                                          fill=cat_frec_ajustada)) +
      ggplot2::geom_rect(color="white", linewidth=2) +
      ggplot2::geom_text(ggplot2::aes(x=3.2,y=ymid,label=lbl),
                         size=4, fontface="bold", color="#34495E") +
      ggplot2::coord_polar(theta="y") + ggplot2::xlim(c(0.8,4)) +
      ggplot2::scale_fill_manual(values=pal_riesgo, guide="none") +
      ggplot2::annotate("text", x=0.8, y=0,
                        label=paste0(scales::comma(sum(d$n)),"\ngrupos"),
                        size=5.5, fontface="bold", color="#34495E") +
      ggplot2::labs(caption=CAT_FREC_TXT) +
      ggplot2::theme_void(base_size=13) +
      ggplot2::theme(plot.caption=ggplot2::element_text(color="gray55", hjust=0.5))
    guardar_plot(p,"dsh_donut"); p
  })

  output$dsh_top <- renderPlot({
    req(res())
    total <- nrow(res()$datos_limpios)
    d <- res()$datos_limpios %>% dplyr::count(tipo_ataque, sort=TRUE) %>%
      head(5) %>% dplyr::mutate(lbl=paste0(round(n/total*100,1),"%"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(tipo_ataque,n), n, fill=tipo_ataque)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.65) +
      ggplot2::geom_text(ggplot2::aes(label=lbl), hjust=-0.08, size=3.4,
                         fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque) +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.25))) +
      ggplot2::labs(x=NULL, y="Incidentes") +
      ggplot2::theme_minimal(base_size=12)
    guardar_plot(p,"dsh_top"); p
  })

  output$dsh_ind <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(imp=sum(impacto,na.rm=TRUE)/1e6, .groups="drop") %>%
      dplyr::arrange(dplyr::desc(imp)) %>% head(6)
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(industria,imp), imp, fill=imp)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.65) +
      ggplot2::geom_text(ggplot2::aes(label=paste0(round(imp,0),"M")),
                         hjust=-0.08, size=3.4, fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_gradient(low="#AED6F1", high="#F1948A") +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.25))) +
      ggplot2::labs(x=NULL, y="Impacto (M, escala)") +
      ggplot2::theme_minimal(base_size=12)
    guardar_plot(p,"dsh_ind"); p
  })

  output$dsh_clus <- renderPlot({
    req(res())
    d <- res()$base %>% dplyr::filter(!is.na(cluster)) %>%
      dplyr::count(cluster) %>%
      dplyr::mutate(pct=round(n/sum(n)*100,1),
                    lbl=paste0(n," (",pct,"%)"))
    if (!nrow(d)) { plot_msg("Cluster no disponible"); return(invisible(NULL)) }
    p <- ggplot2::ggplot(d, ggplot2::aes(paste("Cluster",cluster), n, fill=cluster)) +
      ggplot2::geom_col(show.legend=FALSE, width=0.55) +
      ggplot2::geom_text(ggplot2::aes(label=lbl), vjust=-0.4, size=3.6,
                         fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_cluster) +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.2))) +
      ggplot2::labs(x=NULL, y="Grupos") +
      ggplot2::theme_minimal(base_size=12)
    guardar_plot(p,"dsh_clus"); p
  })

  output$dsh_heat <- renderPlot({
    req(res())
    d <- res()$datos_limpios %>%
      dplyr::group_by(industria, tipo_ataque) %>%
      dplyr::summarise(sev=mean(severidad,na.rm=TRUE), .groups="drop")
    p <- ggplot2::ggplot(d, ggplot2::aes(tipo_ataque, industria, fill=sev)) +
      ggplot2::geom_tile(color="white", linewidth=0.6) +
      ggplot2::geom_text(ggplot2::aes(label=round(sev,1)), size=3.4,
                         color=ifelse(d$sev > mean(d$sev),"#78281F","#1B4F72")) +
      ggplot2::scale_fill_gradient(low="#D6EAF8", high="#F5B7B1",
                                   name="Severidad\nmedia") +
      ggplot2::labs(
        title="Severidad media por industria × tipo de ataque",
        subtitle="Rosado intenso = combinaciones con mayor severidad observada",
        x=NULL, y=NULL,
        caption="Valores en escala 1 (leve) – 10 (crítico)") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=30, hjust=1),
                     plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"dsh_heat"); p
  })

  # ── MINERÍA DE TEXTO Y RECOMENDACIONES ──────────────────
  stop_esp <- c("de","la","el","y","en","que","para","con","los","las",
                "una","un","a","es","se","del","al","su","por","más",
                "no","ya","lo","o","como","si","son","también","sus",
                "ha","le","solo","porque","sino","todo","ser",
                "esto","cada","debe","puede","cuando","pero",
                "tienen","tiene","tanto","así","sin","e")

  output$txt_frec <- renderDT({
    req(res())
    if (is.null(res()$texto_abierto)) {
      d <- res()$datos_limpios %>%
        dplyr::count(tipo_ataque, mitigacion, sort=TRUE) %>%
        dplyr::mutate(pct=round(n/sum(n)*100,2)) %>%
        dplyr::rename(`Tipo de ataque`=tipo_ataque,
                      `Método mitigación`=mitigacion,
                      Frecuencia=n, `%`=pct)
      return(datatable(d,
        caption="Frecuencia: combinaciones ataque-mitigación del dataset técnico. Organizational questionnaire not provided — optional module."))
    }
    tryCatch(
      res()$texto_abierto %>%
        tidytext::unnest_tokens(palabra, respuesta) %>%
        dplyr::filter(!palabra %in% stop_esp, nchar(palabra) > 3) %>%
        dplyr::count(palabra, sort=TRUE) %>%
        head(50) %>%
        datatable(caption="50 términos más frecuentes en respuestas abiertas del cuestionario"),
      error=function(e) datatable(data.frame(Error=e$message))
    )
  })

  output$txt_cloud <- renderPlot({
    req(res())
    if (is.null(res()$texto_abierto)) {
      dl <- res()$datos_limpios
      d_freq <- dl %>%
        dplyr::count(tipo_ataque, mitigacion) %>%
        dplyr::mutate(term=paste(tipo_ataque, mitigacion, sep=" + "))
      tryCatch({
        wordcloud::wordcloud(d_freq$term, d_freq$n, max.words=30,
                             colors=RColorBrewer::brewer.pal(8,"Set2"),
                             scale=c(2.5,.5), min.freq=1)
        graphics::title(
          main="Combinaciones: Tipo de ataque × Método de mitigación",
          sub="Organizational questionnaire not provided — optional module",
          cex.main=1.1, cex.sub=0.8, col.sub="gray50")
      }, error=function(e) {
        graphics::plot.new()
        graphics::text(0.5,0.5,paste("Error:",e$message),col="red")
      })
      return()
    }
    tryCatch({
      texto <- paste(res()$texto_abierto$respuesta, collapse=" ")
      corpus <- tm::Corpus(tm::VectorSource(texto))
      corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
      corpus <- tm::tm_map(corpus, tm::removePunctuation)
      corpus <- tm::tm_map(corpus, tm::removeNumbers)
      corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("spanish"))
      corpus <- tm::tm_map(corpus, tm::stripWhitespace)
      m   <- as.matrix(tm::TermDocumentMatrix(corpus))
      v   <- sort(rowSums(m), decreasing=TRUE)
      d   <- data.frame(word=names(v), freq=v)
      wordcloud::wordcloud(d$word, d$freq, max.words=80,
                           colors=RColorBrewer::brewer.pal(8,"Set2"),
                           scale=c(4,.5), min.freq=1)
    }, error=function(e) {
      graphics::plot.new()
      graphics::text(0.5,0.5,paste("Error en nube:\n",e$message),col="red")
    })
  })

  output$txt_bigram <- renderPlot({
    req(res())
    tryCatch({
      if (is.null(res()$texto_abierto)) {
        dl <- res()$datos_limpios
        d <- dl %>%
          dplyr::count(tipo_ataque, mitigacion, sort=TRUE) %>%
          dplyr::mutate(bigrama=paste(tipo_ataque, "→", mitigacion)) %>%
          head(12)
        p <- ggplot2::ggplot(d, ggplot2::aes(reorder(bigrama,n), n, fill=n)) +
          ggplot2::geom_col(show.legend=FALSE) +
          ggplot2::geom_text(ggplot2::aes(label=scales::comma(n)), hjust=-0.1,
                             size=3.2, color="#4A4A4A") +
          ggplot2::scale_fill_gradient(low="#D6EAF8", high="#85C1E9") +
          ggplot2::coord_flip(clip="off") +
          ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.25))) +
          ggplot2::labs(title="Combinaciones más frecuentes: Ataque → Mitigación",
                        subtitle="Organizational questionnaire not provided — optional module",
                        x=NULL, y="Frecuencia") +
          ggplot2::theme_minimal(base_size=12) +
          ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))
        return(p)
      }
      texto_df <- res()$texto_abierto %>%
        dplyr::mutate(id=dplyr::row_number())
      bigrams <- texto_df %>%
        tidytext::unnest_ngrams(bigrama, respuesta, n=2) %>%
        tidyr::separate(bigrama, c("w1","w2"), sep=" ") %>%
        dplyr::filter(!w1 %in% stop_esp, !w2 %in% stop_esp,
                      nchar(w1)>3, nchar(w2)>3) %>%
        dplyr::count(w1, w2, sort=TRUE) %>%
        dplyr::mutate(bigrama=paste(w1, w2)) %>%
        head(15)
      p <- ggplot2::ggplot(bigrams, ggplot2::aes(reorder(bigrama,n), n, fill=n)) +
        ggplot2::geom_col(show.legend=FALSE) +
        ggplot2::geom_text(ggplot2::aes(label=n), hjust=-0.2, size=3.5, color="#4A4A4A") +
        ggplot2::scale_fill_gradient(low="#E8E6FA", high="#C5C3F0") +
        ggplot2::coord_flip(clip="off") +
        ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.2))) +
        ggplot2::labs(title="Bigramas más frecuentes en respuestas de expertos",
                      subtitle="Pares de palabras que aparecen juntas con mayor frecuencia",
                      x=NULL, y="Frecuencia") +
        ggplot2::theme_minimal(base_size=12) +
        ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))
      p
    }, error=function(e) plot_msg(paste("Error bigrama:",e$message),"red"))
  })

  output$txt_citas <- renderDT({
    req(res())
    if (is.null(res()$texto_abierto)) {
      return(datatable(data.frame(
        Nota="Organizational questionnaire not provided — optional module.")))
    }
    labs_preg <- c(
      abierta_1="Políticas más efectivas",
      abierta_2="Mejoras recomendadas",
      abierta_3="Principales dificultades",
      abierta_4="Políticas menos efectivas"
    )
    res()$texto_abierto %>%
      dplyr::mutate(
        Dimensión = dplyr::recode(pregunta, !!!labs_preg),
        `Respuesta del experto` = respuesta
      ) %>%
      dplyr::select(Dimensión, `Respuesta del experto`) %>%
      dplyr::filter(!is.na(`Respuesta del experto`)) %>%
      datatable(
        caption="Respuestas literales de expertos en ciberseguridad (cuestionario de efectividad)",
        options=list(scrollX=TRUE, pageLength=8,
                     columnDefs=list(list(width="70%", targets=1))),
        rownames=FALSE)
  })

  # ── PANEL DE RECOMENDACIONES ───────────────────────────────
  output$rec_panel <- renderUI({
    req(res())

    dl  <- res()$datos_limpios
    bm  <- res()$base
    br  <- res()$base_res
    enc <- res()$enc_limpia
    tc  <- res()$tbl_coef

    top_ataque <- dl %>%
      dplyr::count(tipo_ataque, sort=TRUE) %>%
      dplyr::mutate(exito=sapply(tipo_ataque, function(a)
        mean(dl$exito[dl$tipo_ataque==a], na.rm=TRUE)*100))

    top_ind <- bm %>%
      dplyr::group_by(industria) %>%
      dplyr::summarise(
        frecuencia=sum(n_incidentes), sev=mean(severidad_media,na.rm=TRUE),
        .groups="drop") %>%
      dplyr::arrange(dplyr::desc(frecuencia))

    mit_eficaz <- dl %>%
      dplyr::group_by(mitigacion) %>%
      dplyr::summarise(exito_med=mean(exito,na.rm=TRUE)*100, n=dplyr::n(),
                       .groups="drop") %>%
      dplyr::arrange(exito_med)

    dist_cat <- br %>%
      dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(pct=round(n/sum(n)*100,1))

    pct_alto <- dist_cat$pct[dist_cat$cat_frec_ajustada=="Alto"]
    if (length(pct_alto)==0) pct_alto <- 0

    coefs_sig <- if (!is.null(tc) && nrow(tc)>0) {
      tc %>% dplyr::filter(p.value < 0.05, term != "(Intercept)") %>%
        dplyr::arrange(desc(abs(log(estimate)))) %>% head(5)
    } else data.frame()

    temas_expertos <- if (!is.null(enc)) {
      list(
        efectivas  = res()$texto_abierto %>%
          dplyr::filter(pregunta=="abierta_1") %>%
          dplyr::pull(respuesta) %>% head(3),
        mejoras    = res()$texto_abierto %>%
          dplyr::filter(pregunta=="abierta_2") %>%
          dplyr::pull(respuesta) %>% head(3),
        dificultad = res()$texto_abierto %>%
          dplyr::filter(pregunta=="abierta_3") %>%
          dplyr::pull(respuesta) %>% head(2)
      )
    } else NULL

    rec_criticas <- list()
    rec_importantes <- list()
    rec_preventivas <- list()

    at1 <- top_ataque$tipo_ataque[1]
    ex1 <- round(top_ataque$exito[1],1)
    rec_criticas[[1]] <- paste0(
      "<b>Política anti-", at1, ":</b> Es el vector de ataque más frecuente (",
      scales::comma(top_ataque$n[1])," incidentes, ", ex1,"% de éxito). ",
      if (at1=="Phishing") "Implementar entrenamiento obligatorio en concienciación, simulacros de phishing y filtros de correo avanzados (SPF, DKIM, DMARC)."
      else if (at1=="DDoS") "Implementar protección anti-DDoS en capa 7, rate limiting y CDN con capacidad de absorción de tráfico malicioso."
      else if (at1=="Ransomware") "Implementar copias de seguridad 3-2-1, segmentación de red y respuesta a incidentes con plan de recuperación probado."
      else if (at1=="Malware") "Implementar EDR (Endpoint Detection & Response), lista blanca de aplicaciones y análisis de comportamiento."
      else paste0("Reforzar controles específicos contra ", at1, " con monitoreo continuo y respuesta automatizada.")
    )

    if (nrow(top_ataque)>1) {
      at2 <- top_ataque$tipo_ataque[2]
      rec_criticas[[2]] <- paste0(
        "<b>Refuerzo contra ", at2, ":</b> Segundo ataque más frecuente con ",
        round(top_ataque$exito[2],1),"% de tasa de éxito. ",
        if (at2=="SQL Injection") "Implementar consultas parametrizadas, WAF y auditorías de código regulares."
        else if (at2=="Brute Force") "Implementar MFA obligatorio, bloqueo de cuentas tras intentos fallidos y CAPTCHA."
        else if (at2=="Cross-Site Scripting") "Implementar Content Security Policy (CSP), validación de entradas y sanitización de salidas."
        else if (at2=="Zero-Day Exploit") "Implementar patch management acelerado, EDR con IA y threat intelligence en tiempo real."
        else paste0("Implementar controles específicos y monitoreo contra ", at2, ".")
      )
    }

    ind1 <- top_ind$industria[1]
    rec_criticas[[3]] <- paste0(
      "<b>Plan de ciberseguridad sectorial — ", ind1, ":</b> Industria con mayor frecuencia de incidentes (",
      scales::comma(top_ind$frecuencia[1]),"). Desarrollar un marco de seguridad específico alineado con ",
      if (ind1=="Finance") "PCI-DSS, SOX y regulaciones bancarias locales."
      else if (ind1=="Healthcare") "HIPAA, normativas de protección de datos médicos y seguridad de dispositivos IoT clínicos."
      else if (ind1=="Government") "marcos NIST, ISO 27001 y directivas de seguridad nacional."
      else if (ind1=="Education") "políticas de privacidad de datos de estudiantes y seguridad de plataformas e-learning."
      else if (ind1=="Technology") "DevSecOps, seguridad de API y gestión de identidad privilegiada (PAM)."
      else paste0("estándares internacionales de la industria ", ind1, ".")
    )

    mit1 <- mit_eficaz$mitigacion[1]
    rec_importantes[[1]] <- paste0(
      "<b>Priorizar '", mit1, "' como estrategia de respuesta:</b> Es el método asociado a la menor tasa de éxito del atacante (",
      round(mit_eficaz$exito_med[1],1),"%). Estandarizar este procedimiento en los playbooks de respuesta a incidentes ",
      "y entrenar al equipo SOC en su aplicación rápida y efectiva. (Asociación observacional, no efecto causal.)")

    t_resp_global <- round(mean(dl$t_respuesta_min, na.rm=TRUE), 0)
    rec_importantes[[2]] <- paste0(
      "<b>Reducir el tiempo de respuesta (actual: ", t_resp_global," min promedio):</b> ",
      "Implementar automatización con SOAR (Security Orchestration, Automation and Response), ",
      "alertas en tiempo real con SIEM y protocolos de escalamiento claros. ",
      "Objetivo: reducir MTTR (Mean Time To Respond) en al menos un 30%.")

    if (nrow(coefs_sig)>0) {
      term_top <- coefs_sig$term[1]
      irr_top  <- round(coefs_sig$estimate[1], 3)
      rec_importantes[[3]] <- paste0(
        "<b>Variable asociada identificada por el modelo GLM — '",
        stringr::str_replace_all(term_top, c("industria"="Industria: ","tipo_ataque"="Tipo: ")),
        "' (IRR=", irr_top, "):</b> ",
        if (irr_top > 1) paste0("Se asocia a una frecuencia de incidentes ", round((irr_top-1)*100,1),
          "% mayor. Requiere controles específicos y revisión prioritaria de políticas en este contexto.")
        else paste0("Se asocia a una frecuencia de incidentes ", round((1-irr_top)*100,1),
          "% menor. Analizar qué condiciones lo explican antes de intentar replicarlas.")
      )
    }

    if (!is.null(temas_expertos)) {
      rec_preventivas[[1]] <- paste0(
        "<b>Basado en expertos consultados:</b> Los especialistas identificaron como políticas más efectivas: ",
        paste(substr(temas_expertos$efectivas, 1, 120), collapse=" | "))
      rec_preventivas[[2]] <- paste0(
        "<b>Principales barreras de implementación según expertos:</b> ",
        paste(substr(temas_expertos$dificultad, 1, 150), collapse=" | "))
    } else {
      rec_preventivas[[1]] <- "<b>Gobierno y cumplimiento:</b> Adoptar marcos reconocidos como ISO 27001, NIST CSF y CIS Controls para estructurar las políticas de ciberseguridad con base auditable."
      rec_preventivas[[2]] <- "<b>Capacitación continua:</b> Implementar programas de concienciación trimestrales, simulacros de ataques y certificaciones para el personal de TI."
      rec_preventivas[[3]] <- "<b>Gestión de vulnerabilidades:</b> Establecer ciclos de parcheo mensual, análisis de vulnerabilidades automatizado y pruebas de penetración anuales."
    }

    tagList(
      div(style="background:#F4F9FB;padding:15px;border-radius:8px;margin-bottom:15px;",
        tags$h4(icon("lightbulb"), " Recomendaciones derivadas del análisis",
                style="color:#34495E;margin-top:0;"),
        tags$p(style="color:#7F8C8D;font-size:13px;margin-bottom:0;",
          "Generadas a partir del análisis estadístico del dataset de ",
          strong(scales::comma(nrow(dl))), " incidentes",
          if (!is.null(enc)) paste0(" y ", nrow(enc), " respuestas del cuestionario organizacional.")
          else ". Organizational questionnaire not provided — optional module.")
      ),

      div(style="margin-bottom:20px;",
        div(style="background:#F1948A;color:#1B2631;padding:8px 15px;border-radius:6px 6px 0 0;",
          tags$h5(icon("triangle-exclamation"), " Prioridad exploratoria ALTA — revisar primero",
                  style="margin:0;font-size:14px;font-weight:bold;")),
        div(style="border:2px solid #F1948A;border-top:none;padding:15px;border-radius:0 0 6px 6px;background:#FDF2F0;",
          tags$ol(lapply(rec_criticas, function(r)
            tags$li(HTML(r), style="margin-bottom:10px;line-height:1.6;")))
        )
      ),

      div(style="margin-bottom:20px;",
        div(style="background:#F0B27A;color:#1B2631;padding:8px 15px;border-radius:6px 6px 0 0;",
          tags$h5(icon("shield"), " Prioridad exploratoria MEDIA — revisar en el siguiente ciclo",
                  style="margin:0;font-size:14px;font-weight:bold;")),
        div(style="border:2px solid #F0B27A;border-top:none;padding:15px;border-radius:0 0 6px 6px;background:#FDF6EE;",
          tags$ol(lapply(rec_importantes, function(r)
            tags$li(HTML(r), style="margin-bottom:10px;line-height:1.6;")))
        )
      ),

      div(style="margin-bottom:20px;",
        div(style="background:#7DCEA0;color:#1B2631;padding:8px 15px;border-radius:6px 6px 0 0;",
          tags$h5(icon("shield-halved"), " Medidas PREVENTIVAS — mejora continua",
                  style="margin:0;font-size:14px;font-weight:bold;")),
        div(style="border:2px solid #7DCEA0;border-top:none;padding:15px;border-radius:0 0 6px 6px;background:#F0FAF4;",
          tags$ol(lapply(rec_preventivas, function(r)
            tags$li(HTML(r), style="margin-bottom:10px;line-height:1.6;")))
        )
      ),

      fluidRow(
        column(3, div(style="background:#85C1E9;color:#1B2631;padding:12px;border-radius:8px;text-align:center;",
          tags$h3(paste0(pct_alto,"%"), style="margin:0;"),
          tags$p("Tercil superior de frec. ajustada", style="margin:0;font-size:12px;"))),
        column(3, div(style="background:#C5C3F0;color:#1B2631;padding:12px;border-radius:8px;text-align:center;",
          tags$h3(paste0(round(mean(dl$exito,na.rm=TRUE)*100,1),"%"), style="margin:0;"),
          tags$p("Tasa éxito ataques", style="margin:0;font-size:12px;"))),
        column(3, div(style="background:#F0B27A;color:#1B2631;padding:12px;border-radius:8px;text-align:center;",
          tags$h3(paste0(t_resp_global," min"), style="margin:0;"),
          tags$p("MTTR actual (promedio)", style="margin:0;font-size:12px;"))),
        column(3, div(style="background:#76D7C4;color:#1B2631;padding:12px;border-radius:8px;text-align:center;",
          tags$h3(top_ataque$tipo_ataque[1], style="margin:0;font-size:16px;"),
          tags$p("Ataque más frecuente", style="margin:0;font-size:12px;")))
      ),

      br(),
      div(style="font-size:11px;color:#95A5A6;border-top:1px solid #E5E8E8;padding-top:8px;",
        icon("circle-info"), " Recomendaciones generadas a partir del análisis KDD, el modelo GLM y ",
        "las respuestas del cuestionario. Las relaciones estimadas son asociaciones observacionales; ",
        "no reemplazan el juicio experto.")
    )
  })

  # ── CONCLUSIÓN (visual + texto) ───────────────────────────
  output$con_vb_modelo <- renderValueBox({
    req(res())
    valueBox(res()$tipo_mdl, "Modelo estadístico aplicado",
             icon=icon("calculator"), color="purple")
  })
  output$con_vb_inc <- renderValueBox({
    req(res())
    valueBox(scales::comma(res()$resumen_mdl$total_incidentes),
             "Incidentes analizados",
             icon=icon("shield-halved"), color="blue")
  })
  output$con_vb_efx <- renderValueBox({
    req(res())
    if (isTRUE(res()$tiene_encuesta) && !is.null(res()$scores_enc)) {
      valueBox(paste0(round(res()$scores_enc$EIglobal,2)," / 5"),
               "EIglobal — optional organizational module",
               icon=icon("chart-line"), color="green")
    } else {
      valueBox("Not available",
               "Optional Organizational Module",
               icon=icon("clipboard"), color="yellow")
    }
  })
  output$con_vb_exito <- renderValueBox({
    req(res())
    valueBox(scales::percent(res()$resumen_mdl$tasa_exito_global, accuracy=0.1),
             "Tasa de ataques exitosos",
             icon=icon("skull-crossbones"), color="red")
  })

  output$con_gauge <- renderPlot({
    req(res())
    if (!isTRUE(res()$tiene_encuesta) || is.null(res()$scores_enc))
      return(plot_msg("Organizational questionnaire not provided — optional module"))
    val <- round(res()$scores_enc$EIglobal, 2)   # 1–5
    pct <- max(min(val/5, 1), 0)
    col_val <- if (val >= 3.6) "#82E0AA" else if (val >= 2.1) "#F8C471" else "#F1948A"
    nivel   <- if (val >= 3.6) "ALTA" else if (val >= 2.1) "MEDIA" else "BAJA"
    d <- data.frame(grupo=factor(c("valor","resto"), levels=c("resto","valor")),
                    n=c(pct, 1-pct))
    p <- ggplot2::ggplot(d, ggplot2::aes(x=2, y=n, fill=grupo)) +
      ggplot2::geom_col(width=1, color="white", linewidth=2) +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::xlim(0.5, 2.5) +
      ggplot2::scale_fill_manual(values=c(valor=col_val, resto="#EAEDED"),
                                 guide="none") +
      ggplot2::annotate("text", x=0.5, y=0, label=paste0(val, " / 5"),
                        size=10, fontface="bold", color="#34495E") +
      ggplot2::annotate("text", x=1.05, y=0,
                        label=paste0("Efectividad percibida ", nivel),
                        size=4.5, color="#7F8C8D") +
      ggplot2::labs(
        title="EIglobal — Optional Organizational Module",
        subtitle="Anillo lleno = 5/5 | Verde=Alta (≥3.6) · Melocotón=Media (≥2.1) · Rosado=Baja",
        caption=paste0("Promedio de las 5 dimensiones: FI, SI, IE, RD y PG. ", cap_enc())) +
      ggplot2::theme_void(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", hjust=0.5, color="#34495E"),
                     plot.subtitle=ggplot2::element_text(hjust=0.5, color="gray50", size=10),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0.5))
    guardar_plot(p,"con_gauge"); p
  })

  output$con_donut <- renderPlot({
    req(res())
    d <- res()$base_res %>%
      dplyr::count(cat_frec_ajustada) %>%
      dplyr::filter(!is.na(cat_frec_ajustada)) %>%
      dplyr::mutate(
        pct = round(n/sum(n)*100,1),
        lbl = paste0(cat_frec_ajustada, "\n", pct, "%"),
        ymax = cumsum(n),
        ymin = dplyr::lag(ymax, default=0),
        ymid = (ymax+ymin)/2
      )
    p <- ggplot2::ggplot(d, ggplot2::aes(ymax=ymax, ymin=ymin,
                                          xmax=4, xmin=2.4, fill=cat_frec_ajustada)) +
      ggplot2::geom_rect(color="white", linewidth=2) +
      ggplot2::geom_text(ggplot2::aes(x=3.2, y=ymid, label=lbl),
                         size=4.2, fontface="bold", color="#34495E") +
      ggplot2::coord_polar(theta="y") +
      ggplot2::xlim(c(0.8, 4)) +
      ggplot2::scale_fill_manual(values=pal_riesgo, guide="none") +
      ggplot2::annotate("text", x=0.8, y=0,
                        label=paste0(scales::comma(sum(d$n)), "\ngrupos"),
                        size=6, fontface="bold", color="#34495E") +
      ggplot2::labs(
        title="Composición por tercil de frecuencia ajustada (GLM)",
        subtitle="Cada grupo = combinación año × país × industria × tipo de ataque",
        caption=CAT_FREC_TXT) +
      ggplot2::theme_void(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", hjust=0.5, color="#34495E"),
                     plot.subtitle=ggplot2::element_text(hjust=0.5, color="gray50", size=10),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0.5))
    guardar_plot(p,"con_donut"); p
  })

  output$con_dims <- renderPlot({
    req(res())
    if (is.null(enc_dims()))
      return(plot_msg("Organizational questionnaire not provided — optional module"))
    d <- enc_dims() %>%
      dplyr::mutate(Dim = gsub("\n"," — ", Dimensión))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(Dim, Media), Media, color=Dim)) +
      ggplot2::geom_segment(ggplot2::aes(xend=reorder(Dim, Media), y=0, yend=Media),
                            linewidth=2.5, alpha=0.6) +
      ggplot2::geom_point(size=7) +
      ggplot2::geom_text(ggplot2::aes(label=round(Media,2)),
                         color="#34495E", size=3.4, fontface="bold", vjust=-1.3) +
      ggplot2::geom_hline(yintercept=3, linetype="dashed", color="gray55") +
      ggplot2::annotate("text", x=0.6, y=3.08, label="Punto neutro (3)",
                        size=3, color="gray55", hjust=0) +
      ggplot2::scale_color_manual(values=c("#85C1E9","#F5B7B1","#A9DFBF",
                                           "#FAD7A0","#C39BD3","#76D7C4"),
                                  guide="none") +
      ggplot2::scale_y_continuous(limits=c(0,5.4), breaks=1:5) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title="Puntuación media por dimensión de efectividad (1–5)",
        subtitle="Optional Organizational Module: no participa en el modelo estadístico",
        x=NULL, y="Media (escala Likert 1–5)",
        caption=paste0("FI=Frecuencia · SI=Severidad · IE=Impacto económico · RD=Respuesta/Recuperación · PG=Percepción global. ",
                       cap_enc())) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"con_dims"); p
  })

  output$con_top <- renderPlot({
    req(res())
    total <- nrow(res()$datos_limpios)
    d <- res()$datos_limpios %>%
      dplyr::count(tipo_ataque, sort=TRUE) %>%
      head(5) %>%
      dplyr::mutate(pct=round(n/total*100,1),
                    lbl=paste0(scales::comma(n)," (",pct,"%)"))
    p <- ggplot2::ggplot(d, ggplot2::aes(reorder(tipo_ataque, n), n, fill=tipo_ataque)) +
      ggplot2::geom_col(width=0.65, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=lbl), hjust=-0.06,
                         size=3.6, fontface="bold", color="#4A4A4A") +
      ggplot2::scale_fill_manual(values=pal_ataque) +
      ggplot2::coord_flip(clip="off") +
      ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,.32))) +
      ggplot2::labs(
        title="Los 5 vectores de ataque más frecuentes del estudio",
        subtitle="Prioridades de intervención para las políticas de ciberseguridad",
        x=NULL, y="Número de incidentes",
        caption="Porcentaje calculado sobre el total de incidentes analizados") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold", color="#34495E"),
                     plot.subtitle=ggplot2::element_text(color="gray50"),
                     plot.caption=ggplot2::element_text(color="gray55", hjust=0))
    guardar_plot(p,"con_top"); p
  })

  output$conclusion <- renderPrint({
    req(res())
    r  <- res()$resumen_mdl
    sc <- res()$scores_enc
    r2 <- tryCatch(cor(res()$base_res$n_incidentes,
                       res()$base_res$ajustados, use="complete.obs")^2,
                   error=function(e) NA)

    cat("══════════════════════════════════════════════════════\n")
    cat("  CONCLUSIÓN — ANÁLISIS DE INCIDENTES DE\n")
    cat("  CIBERSEGURIDAD Y APOYO A LA DECISIÓN\n")
    cat("══════════════════════════════════════════════════════\n\n")
    cat("── Trazabilidad ────────────────────────────────────\n")
    cat("  Framework version:", FRAMEWORK_VERSION, "\n")
    cat("  Random seed      :", RANDOM_SEED, "\n\n")
    cat("── Arquitectura analítica ──────────────────────────\n")
    cat("  Technical Core                : EDA + PCA + K-Means + GLM\n")
    cat("  Optional Organizational Module: FI, SI, IE, RD, PG, EIglobal\n")
    cat("                                  (evidencia complementaria,\n")
    cat("                                   fuera del modelo estadístico)\n\n")
    cat("── Modelo aplicado ─────────────────────────────────\n")
    cat("  Tipo      :", r$modelo, "\n")
    cat("  AIC       :", ifelse(is.na(r$aic), "NA (cuasi-verosimilitud)", round(r$aic, 2)), "\n")
    cat("  BIC       :", ifelse(is.na(r$bic), "NA (cuasi-verosimilitud)", round(r$bic, 2)), "\n")
    cat("  Dispersión:", round(r$dispersion, 3), "—", r$dispersion_status, "\n")
    cat("    · Pruebas direccionales sobre las colas inferior y superior\n")
    cat("      de X² de Pearson, con α = 0.05.\n")
    cat("  k final   :", r$k_selected, "\n")
    cat("    · ", K_FINAL_RATIONALE, "\n", sep="")
    cat("    · Silhouette k=", r$k_selected, ": ",
        round(res()$k_selected_sil, 4), "\n", sep="")
    cat("    · Máximo empírico: k=", res()$k_sil_max,
        " (Silhouette=", round(res()$k_sil_max_value, 4), ")\n", sep="")
    cat("    · Silhouette partición final (todas las celdas): ",
        round(res()$sil_final_full, 4), "\n", sep="")
    if (!is.null(res()$sensibilidad)) {
      st <- res()$sensibilidad$estabilidad_terciles
      cat("── Sensibilidad (v7.3.3) ───────────────────────────\n")
      cat("  Terciles total vs medio: rho =", round(st$spearman_rho, 3),
          "| mismo tercil =", round(st$pct_same_tertile, 1), "%",
          "| kappa =", round(st$cohen_kappa, 3), "\n")
      cat("    · Los terciles son rankings exploratorios dependientes\n")
      cat("      de la especificación del modelo.\n")
    }
    cat("  r² Pearson (observado vs ajustado):", round(r2, 4), "\n")
    cat("    · r² = cor(observado, ajustado)²; ajuste dentro de muestra.\n")
    cat("    · No es el R² de un modelo lineal ni validación predictiva.\n\n")
    cat("── Dataset de incidentes ───────────────────────────\n")
    cat("  Total incidentes      :", scales::comma(r$total_incidentes), "\n")
    cat("  Impacto total (M)     :",
        scales::comma(round(r$impacto_total/1e6,1)), "\n")
    cat("  Severidad media (1-10):", round(r$severidad_global,2), "\n")
    cat("  Tasa de éxito ataques :",
        scales::percent(r$tasa_exito_global, accuracy=0.1), "\n\n")

    if (!is.null(sc)) {
      cat("── Optional Organizational Module ──────────────────\n")
      cat("  N° respondentes  :", sc$n, "\n")
      cat("  FI Frecuencia    :", round(sc$FI, 2), "/ 5\n")
      cat("  SI Severidad     :", round(sc$SI, 2), "/ 5\n")
      cat("  IE Impacto Econ. :", round(sc$IE, 2), "/ 5\n")
      cat("  RD Respuesta     :", round(sc$RD, 2), "/ 5\n")
      cat("  PG Percepción    :", round(sc$PG, 2), "/ 5\n")
      cat("  EIglobal         :", round(sc$EIglobal, 2), "/ 5\n\n")
    } else {
      cat("── Optional Organizational Module ──────────────────\n")
      cat("  Organizational questionnaire not provided — optional module.\n")
      cat("  No FI/SI/IE/RD/PG/EIglobal values were derived from technical data.\n\n")
    }

    cat("── Metodología KDD ─────────────────────────────────\n")
    cat("  Selección → Preprocesamiento → Transformación\n")
    cat("  → Minería (ACP + K-Means +", r$modelo, ")\n")
    cat("  → Interpretación (valores ajustados + categoría\n")
    cat("     exploratoria de frecuencia por terciles)\n\n")
    cat("── Nota metodológica ───────────────────────────────\n")
    cat(" ", CAT_FREC_TXT, "\n")
    cat("══════════════════════════════════════════════════════\n")
  })

  # ── PUBLICACIÓN: solo exportación a PDF/JPG/HTML ──────────
  pub_files <- reactiveVal(NULL)

  # ── [v7.3.3] SENSIBILIDAD ─────────────────────────────────
  dt_sens <- function(x, digits=3) {
    x <- as.data.frame(x)
    num <- names(x)[vapply(x, is.numeric, logical(1))]
    d <- DT::datatable(x, rownames=FALSE,
                       options=list(dom="t", scrollX=TRUE, pageLength=100))
    if (length(num)) d <- DT::formatRound(d, num, digits)
    d
  }
  sens <- reactive({ r <- req(res()); req(r$sensibilidad); r$sensibilidad })
  output$sens_fig11 <- renderPlot({ plot_figure11(sens()) }, res=96)
  # [v7.3.3] Resumen visual
  output$vis_dashboard <- renderPlot({ plot_visual_summary(req(res())) }, res=96)
  output$dl_vis_pdf <- downloadHandler(
    filename=function() "Visual_summary.pdf",
    content=function(file) ggplot2::ggsave(file, plot_visual_summary(req(res())),
                                           width=13, height=10, device="pdf"))
  output$dl_vis_jpg <- downloadHandler(
    filename=function() "Visual_summary.jpg",
    content=function(file) ggplot2::ggsave(file, plot_visual_summary(req(res())),
                                           width=13, height=10, dpi=600, device="jpeg", bg="white"))
  output$sens_impacto <- renderDT(dt_sens(sens()$resumen_impacto))
  output$sens_terciles <- renderDT(dt_sens(sens()$estabilidad_terciles, 2))
  output$sens_tabla_terciles <- renderDT(dt_sens(sens()$tabla_terciles, 0))
  output$sens_silueta <- renderDT(dt_sens(sens()$silueta))
  output$sens_tucker <- renderDT(dt_sens(sens()$tucker))
  output$sens_pca <- renderDT(dt_sens(sens()$pca_var[, c("variante","PC","eigen","var_pct","var_acum")], 2))
  output$dl_fig11_pdf <- downloadHandler(
    filename=function() "Figure_11_sensitivity.pdf",
    content=function(file) ggplot2::ggsave(file, plot_figure11(sens()),
                                           width=11, height=9.5, device="pdf"))
  output$dl_fig11_jpg <- downloadHandler(
    filename=function() "Figure_11_sensitivity.jpg",
    content=function(file) ggplot2::ggsave(file, plot_figure11(sens()),
                                           width=11, height=9.5, dpi=600, device="jpeg", bg="white"))
  observeEvent(res(), cmp_res(NULL))
  observeEvent(input$btn_cmp, {
    r <- req(res())
    withProgress(message="Ajustando Conway–Maxwell–Poisson (puede tardar varios minutos)…", value=0.2, {
      out <- tryCatch(ejecutar_cmp(r$base, r$mdl_formula, r$mdl_final),
        error=function(e) {
          showNotification(paste("COM-Poisson:", conditionMessage(e)), type="error", duration=15)
          NULL })
      cmp_res(out)
    })
    if (!is.null(cmp_res()))
      showNotification("COM-Poisson listo: resultados en esta pestaña y en el reporte Excel.",
                       type="message", duration=8)
  })
  output$sens_cmp_resumen <- renderDT({ req(cmp_res()); dt_sens(cmp_res()$resumen) })
  output$sens_cmp_comp <- renderDT({ req(cmp_res()); dt_sens(cmp_res()$comparacion) })

  observeEvent(input$btn_publicar, {
    req(res())
    withProgress(message="Exportando gráficos de publicación…", value=0, {
      archivos <- tryCatch(
        generar_publicacion(res()),
        error=function(e) {
          showNotification(paste("Error al exportar:", conditionMessage(e)),
                           type="error", duration=15)
          NULL
        }
      )
      pub_files(archivos)
    })
    if (!is.null(pub_files()))
      showNotification("Gráficos de publicación exportados en outputs/article_figures/",
                       type="message", duration=8)
  })

  output$pub_estado <- renderUI({
    if (is.null(pub_files())) {
      return(tags$div(class="alert",
        style="background:#FEF9E7;border:1px solid #F9E79F;color:#7D6608;",
        icon("circle-info"),
        " Aún no se han generado los gráficos. Cargue el CSV (y opcionalmente el XLSX) ",
        "y pulse el botón ", strong("Generar y guardar gráficos de publicación"), "."))
    }
    n_ok <- sum(grepl("Exportado", pub_files()$Estado))
    tags$div(class="alert",
      style="background:#D5F5E3;border:1px solid #A9DFBF;color:#1E8449;",
      icon("check"),
      sprintf(" Exportación completada: %d elementos generados en PDF, JPG y HTML dentro de ", n_ok),
      code("outputs/article_figures/"), ".")
  })

  output$pub_archivos <- renderDT({
    req(pub_files())
    datatable(pub_files(),
      caption="Rutas de los archivos exportados (relativas al directorio de la app)",
      options=list(scrollX=TRUE, pageLength=15),
      rownames=FALSE)
  })

} # fin server

# ── 5. LANZAR APP ────────────────────────────────────────────
shinyApp(ui, server)