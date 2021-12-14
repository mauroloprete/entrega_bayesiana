## ----include = FALSE-----------------------------------------------------

if(!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  magrittr,
  tidytable,
  knitr,
  kableExtra,
  stringr,
  rstanarm,
  data.table,
  ggplot2,
  magrittr,
  viridis,
  ggridges,
  hrbrthemes,
  rlang,
  e1071,
  fastDummies,
  broom.mixed,
  tidyselect,
  bayesplot
 )


options(kableExtra.latex.load_packages = FALSE)

 knitr::write_bib(
   .packages(),
   here(
     "Documento",
     "biblio",
     "bib.bib"
   )
 )

# Cargar los datos

fread(
  here(
    "data",
    "coffee.csv"
  )
) %>%
select.(
  V1,
  country_of_origin,
  company,
  species,
  number_of_bags,
  in_country_partner,
  grading_date,
  owner_1,
  variety,
  processing_method,
  aroma,
  flavor,
  aftertaste,
  acidity,
  body,
  balance,
  uniformity,
  clean_cup,
  sweetness,
  cupper_points,
  total_cup_points,
  moisture,
  color,
  quakers,
  expiration
) %>%
mutate.(
    across.(
        where(is.character),
        ~ as.factor(.x)
    )
) %>%
filter.(
  total_cup_points > 0
) %>%
assign(
    "data",
    .,
    envir = .GlobalEnv
)

# Para poder recuperar el script de los chunk

knitr::purl(
  here::here(
    "Documento",
    "Doc.Rnw"
  )
)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)


    plots <- c(list(...), plotlist)

    numPlots = length(plots)


    if (is.null(layout)) {

        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
        ncol = cols, nrow = ceiling(numPlots/cols))
      }

    if (numPlots==1) {
        print(plots[[1]])

      } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
              layout.pos.col = matchidx$col))
          }
      }
  }



## ----echo = FALSE--------------------------------------------------------
data %>%
    summarise.(
        across.(
            c(
                where(is.numeric),
                - V1
            ),
            .fns = list(
                Minimo = ~ round(
                  min(
                    .x,
                    na.rm = TRUE
                  ),
                  2
                ),
                Media = ~ round(
                  mean(
                    .x,
                    na.rm = TRUE
                  ),
                  2
                ),
                Mediana = ~ round(
                  median(
                    .x,
                    na.rm = TRUE
                  ),
                  2
                ),
                Maximo = ~ round(
                  max(
                    .x,
                    na.rm = TRUE
                  ),
                  2
                ),
                Faltantes = function(x) {
                  round(
                      (
                      sum(is.na(x)) / nrow(.)
                    ) * 100,
                    2
                  )
                },
                Outliers = function(x) {
                  Q1 = quantile(x, probs = 0.25,na.rm = TRUE)
                  Q3 = quantile(x,probs = 0.75,na.rm = TRUE)
                  RI = Q3 - Q1
                  n_ef = sum(!is.na(x))
                  round(
                      100*(
                      sum(
                        x < Q1 - 1.5*RI | x > Q3 + 1.5*RI,
                        na.rm = TRUE
                      )
                    )/n_ef,
                    2
                  )
                },
                Asimetria = function(x) {
                  
                  if(skewness(x,na.rm = T) > 0) {
                    "Positiva"
                  } else if (skewness(x,na.rm = T) < 0) {
                     "Negativa"
                  } else {
                     "Indeterminada"
                  }

                }
            ),
            .names = "{.fn}-{.col}"
        )
    ) %>%
    pivot_longer.(
        names_to = "statVar",
        values_to = "statVal"
    ) %>%
    mutate.(
        stat = str_split(
            statVar,
            "-",
            simplify = TRUE
        )[, 1],
        Variable = str_split(
            statVar,
            "-",
            simplify = TRUE
        )[, 2],
        .keep = "unused"
    ) %>%
    pivot_wider.(
        names_from = "stat",
        values_from = "statVal"
    ) %>%
    mutate.(
      across.(
        .cols = -c(
          Variable,
          Asimetria
        ),
        format,
        digits = 2,
        nsmall = 0
      )
    ) %>%
    relocate.(
      Minimo,
      Media,
      Mediana,
      Maximo,
      Faltantes,
      Outliers,
      Asimetria,
      .after = Variable
    ) %>%
    set_names(
      c(
        "Variable",
        "Min",
        "Media",
        "Mediana",
        "Máx",
        "Faltantes %",
        "Outliers %",
        "Asimetría"
      )
    ) %>%
    kbl(
        booktabs = T,
        caption = "Estadísticas descriptivas para variables númericas"
    ) %>%
    kable_styling(
        latex_options = c(
          "striped",
          "hold_position"
        )
    )


## ----echo = FALSE,warning = FALSE,message=FALSE,fig.cap = "Distribución de totalcup por especie",out.width="85%"----

data %>%
    ggplot(
        aes(
            x = total_cup_points,
            y = species,
            fill = species
        )
    ) + 
    geom_density_ridges2() +
    theme_ipsum() + 
    labs(
        x = "total cup points",
        y = "",
        fill = "Especie"
    ) + 
    scale_fill_viridis(
        discrete = TRUE
    ) +
    scale_color_viridis(
        discrete = TRUE
    ) +
    theme_ridges(
        grid = FALSE
    )




## ----echo=FALSE,warning=FALSE,message=FALSE------------------------------

data %>%
dummy_columns(
  select_columns = c(
      "species",
      "processing_method",
      "color"
    ),
  remove_first_dummy = TRUE,
  ignore_na = TRUE,
  remove_selected_columns = TRUE
) %>%
mutate.(
  across.(
    .cols = c(
      starts_with("species_"),
      starts_with("processing_method_"),
      starts_with("color_")
    ),
    ~ replace_na.(.x,0)
  )
) %>%
set_names(
  stringr::str_trim(
    names(.),
    side = "both"
  )
) %>%
rename_with.(
  ~ tolower(
    gsub(
      "-",
      "_",
      gsub(
        " ",
        "_",
        gsub(" / ","_",.x)
      )
    )
  )
) %>%
mutate.(
  quakers = replace_na.(
    quakers,
    0
  )
) %>%
select.(
  -c(
    starts_with("altitude")
  )
) %>%
  assign(
    "data_modelo",
    .,
    envir = .GlobalEnv
  )


lm(
  total_cup_points ~ species_robusta + 
    processing_method_other +
    processing_method_pulped_natural_honey + 
    processing_method_semi_washed_semi_pulped +
    processing_method_washed_wet +
    color_bluish_green + 
    color_green + 
    color_none +
    species_robusta + 
    number_of_bags + 
    aroma + 
    flavor + 
    aftertaste + 
    acidity + 
    body + 
    balance + 
    uniformity + 
    clean_cup + 
    sweetness + 
    cupper_points + 
    moisture + 
    quakers + 
    country_of_origin,
    data = na.omit(data_modelo)
) %>%
step(
  direction = "both",
  trace = FALSE
) %>%
assign(
  "modelo",
  .,
  envir = .GlobalEnv
)


## ----include = FALSE-----------------------------------------------------

# Modelo1 <- stan_glm(
#  formula(modelo),
#  data = data_modelo,
#  chains = 4,
#  family = gaussian(),
#  iter = 8000,
#  seed = 100
# )

# save(
#  Modelo1,
#  file = here::here(
#    "output",
#    "Modelo1.Rdata"
#  )
# )

load(
  here::here(
    "output",
    "Modelo1.RData"
  )
)




## ----echo=FALSE,out.width="65%",warning=FALSE,message=FALSE,fig.align='center'----

color_scheme_set(scheme = "purple")

pp_check(
  Modelo1,
  plotfun = "stat",
  stat = "median"
)

pp_check(
  Modelo1,
  plotfun = "stat",
  stat = "sd"
)




## ----eval = FALSE,echo = FALSE,out.width = "65%"-------------------------
## 
## c(
##   "aroma",
##   "flavor",
##   "aftertaste",
##   "acidity",
##   "body",
##   "balance",
##   "uniformity",
##   "sweetness",
##   "clean_cup",
##   "cupper_points"
## ) %>%
## combn(4) %>%
## dplyr::tibble() %>%
##    summarize.(
##      across.(
##        .cols = everything(),
##        ~ str_c(
##          .x,
##          collapse = "+"
##        )
##      )
##    ) %>%
##    pivot_longer.(
##      names_to = "rep",
##      values_to = "value"
##    ) %>%
##    pull.(
##      "value"
##    ) %>%
##    map_dfr.(
##       .f = function(x) {
##         lm(
##           formula(
##             paste0(
##               "total_cup_points ~
##               species_robusta +
##               processing_method_other +
##               processing_method_pulped_natural_honey +
##               processing_method_semi_washed_semi_pulped +
##               processing_method_washed_wet +
##               color_bluish_green +
##               color_green +
##               color_none +
##               moisture +
##               quakers + ",
##               x
##             )
##           ),
##           data = na.omit(data_modelo)
##         ) %>%
##         step(
##           direction = "both",
##           trace = FALSE
##         ) %>%
##         formula() %>%
##         assign(
##           "formula",
##           .,
##           envir = parent.env(
##             environment()
##           )
##         )
## 
##         lm(
##           formula(formula),
##           data = na.omit(data_modelo)
##         ) %>%
##         broom::glance() %>%
##         mutate.(
##           formula = paste0(
##             deparse(formula, width.cutoff = 500)
##           )
##         )
##       }
## 
##     ) %>%
##     fwrite.(
##       here(
##         "output",
##         "modelos.csv"
##       )
##     )
## 
## 


## ----echo = FALSE,eval = FALSE,out.width="65%"---------------------------
## 
## set.seed(1234)
## 
## fread.(
##   here(
##     "output",
##     "modelos.csv"
##   )
## ) %>%
## slice_sample.(
##   n = 1
## ) %>%
## assign(
##   "modelo",
##   .,
##   envir = .GlobalEnv
## )
## 
## 
## modelo %<>%
##  mutate.(
##    index = 1:n(),
##    formula = formula,
##    .keep = "none"
##  )
## 
## 
## stan_glm(
##     formula(modelo$formula),
##     data = data_modelo,
##     chains = 4,
##     family = gaussian(),
##     iter = 8000,
##     seed = 100
##   ) %>%
##   assign(
##     "Modelo_1",
##     .,
##     envir = .GlobalEnv
##   )
## 
##   save(
##     Modelo_1,
##     file = here::here(
##       "output",
##       "Modelo_comb.Rdata"
##     )
##   )
## 
## # Gráficos Mediana
## 
## pp_check(
##       Modelo_1,
##       plotfun = "stat",
##       stat = "median"
##     ) %>%
##     assign(
##       "plot_modelo_1",
##       .,
##       envir = .GlobalEnv
##     )
## 
## 
## ggsave(
##   here::here(
##     "output",
##     "Mediana.png"
##   )
## )
## 
## 
## pp_check(
##       Modelo_1,
##       plotfun = "stat",
##       stat = "sd"
##     ) %>%
##     assign(
##       "plot_modelo_1_sd",
##       .,
##       envir = .GlobalEnv
##     )
## 
## ggsave(
##     here::here(
##     "output",
##     "sd.png"
##   ),
##   plot = plot_modelo_1_sd
## )
## 
## 
## 
## pp_check(
##       Modelo_1
##     ) %>%
##     assign(
##       "plotcheck_Modelo_1",
##       .,
##       envir = .GlobalEnv
##     )
## 
## 
## save(
##   plotcheck_Modelo_1,
##   file = here(
##     "output",
##     "plot_check.Rdata"
##   )
## )
## 
## 
## 
## posterior_predict(
##       Modelo_1,
##       draws = 1000
##     ) %>%
##     assign(
##       "yrep_Modelo_1",
##       .,
##       envir = .GlobalEnv
##     )
## 
##     ppc_scatter_avg(
##       data_modelo$total_cup_points,
##       yrep_Modelo_1,
##       size = 2.5,
##       alpha = 0.8
##     ) %>%
##     assign(
##       "plot_yrep_Modelo_1",
##       .,
##       envir = .GlobalEnv
##     )
## 
## 
## save(
##   plot_yrep_Modelo_1,
##   file = here(
##     "output",
##     "yrep.Rdata"
##   )
## )
## 


## ----echo = FALSE--------------------------------------------------------
Modelo_1 %>%
  summary() %>%
  as.data.frame() %>%
  mutate.(
    Variable = row.names(.),
    .before = everything()
  ) %>%
  mutate.(
    across.(
      .cols = where(is.numeric),
      ~ round(.x,2)
    )
  ) %>%
  select.(
    -mcse,
    -`50%`
  ) %>%
  set_names(
    "Variable",
    "Media MCMC",
    "Desv.Std",
    "Int.10%",
    "Int.90%",
    "n_eff",
    "Rhat"
  ) %>%
  kbl(
    booktabs = T, 
    caption = "Estimación puntual y errores estandár, intervalos de confianza y indices de convergencia"
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 8.5
  )


## ----echo = FALSE, out.width = "65%", warning = FALSE,message = FALSE,fig.align = 'center', fig.cap = "Ajuste Mediana yrep vs observada"----

include_graphics(
  here(
    "output",
    "Mediana.png"
  )
)




## ----echo=FALSE,out.width="65%",warning=FALSE,message=FALSE,fig.align='center',fig.cap = "Ajuste desvió estándar yrep vs observada"----

color_scheme_set(scheme = "purple")


include_graphics(
  here(
    "output",
    "sd.png"
  )
)




## ----echo = FALSE,out.width="65%"----------------------------------------


load(
  here::here(
    "output",
    "yrep.RData"
  )
)

plot_yrep_Modelo_1



## ----echo = F------------------------------------------------------------
tidytable(
  Variable = names(data),
  Tipo = map_chr.(data,class)
) %>%
mutate.(
  Tipo = case_when.(
    Tipo == "integer" ~ "Númerica",
    Tipo == "numeric" ~ "Númerica",
    Tipo == "factor" ~ "Categoríca"
  )
) %>%
kbl(
    booktabs = T, 
    caption = "Metadata"
) %>%
kable_styling(
  latex_options = c("striped", "hold_position"),
  font_size = 8.5
)


