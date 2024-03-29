#' Visualizes regional disaggregated estimates on a map
#'
#' Function \code{map_plot} creates spatial visualizations of the estimates
#' obtained by small area estimation methods.
#'
#' @param object an object of type \code{saeTrafo}, containing the estimates to
#' be visualized.
#' @param MSE optional logical. If \code{TRUE}, the MSE is also visualized.
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the CV is also visualized.
#' Defaults to \code{FALSE}.
#' @param map_obj an \code{sf} object (map object) as defined by the
#' \pkg{sf} package on which the data should be visualized.
#' @param map_dom_id a character string containing the name of a variable in
#' \code{map_obj} that indicates the domains.
#' @param map_tab a \code{data.frame} object with two columns that match the
#' domain variable from the census data set (first column) with the domain
#' variable in the map_obj (second column). This should only be used if the IDs
#' in both objects differ.
#' @param color a \code{vector} of length 2 defining the lowest and highest
#' color in the plots.
#' @param scale_points a numeric vector of length two. This
#' scale will be used for every plot.
#' @param guide character passed to \code{scale_fill_gradient} from
#' \pkg{ggplot2}. Possible values are "none", "colourbar", and "legend".
#' Defaults to "colourbar".
#' @param return_data if set to \code{TRUE}, a fortified data frame including
#' the map data as well as the chosen indicators is returned. Customized maps
#' can easily be obtained from this data frame via the package \pkg{ggplot2}.
#' Defaults to \code{FALSE}.
#' @return Creates the plots demanded and, if selected, a fortified data.frame
#' containing the mapdata and chosen indicators.
#' @seealso \code{\link[sf]{sf}}, \code{\link{NER_Trafo}},
#' \code{\link{saeTrafoObject}}
#' @examples
#'
#' \donttest{
#' # Examples for creating maps to visualize the saeTrafo estimates
#'
#' # Load Data
#' data("eusilcA_smp")
#' data("pop_area_size")
#' data("pop_mean")
#' data("pop_cov")
#'
#' # Nested error regression model
#' NER_model <- NER_Trafo(fixed = eqIncome ~ gender + eqsize + cash +
#'                        self_empl + unempl_ben + age_ben + surv_ben +
#'                        sick_ben + dis_ben + rent + fam_allow + house_allow +
#'                        cap_inv + tax_adj,
#'                        smp_domains = "district",
#'                        pop_area_size = pop_area_size,
#'                        pop_mean = pop_mean, pop_cov = pop_cov,
#'                        smp_data = eusilcA_smp, MSE = TRUE)
#'
#' # Load shape file
#' load_shapeaustria()
#'
#' # Example 1: Map plots with uncertainty plots (for MSE and CV)
#' map_plot(NER_model, MSE = TRUE, CV = TRUE, map_obj = shape_austria_dis,
#'          map_dom_id = "PB")
#'
#' # Example 2: Personalize map plot for point estimates
#' map_plot(NER_model, map_obj = shape_austria_dis, map_dom_id = "PB",
#'          color = c("white", "darkblue"),
#'          scale_points = c(0, max(NER_model$ind$Mean)))
#'
#' # Example 3: More options to personalize map plot by using return_data = TRUE
#' # and ggplot2
#' require(ggplot2)
#' library(ggplot2)
#' data_plot <- map_plot(NER_model, map_obj = shape_austria_dis, map_dom_id = "PB",
#'                       return_data = TRUE)
#' ggplot(data_plot, aes(long = NULL, lat = NULL,
#'                       group = "PB", fill = Mean))+
#'        geom_sf(color = "black") +
#'        theme_void() +
#'        ggtitle("Personalized map") +
#'        scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
#'                             midpoint = 20000)
#'
#' # Example 4: Create a suitable mapping table to use numerical identifiers of
#' # the shape file
#'
#' # First find the right order
#' dom_ord <- match(shape_austria_dis$PB, NER_model$ind$Domain)
#'
#' #Create the mapping table based on the order obtained above
#' map_tab <- data.frame(pop_data_id = NER_model$ind$Domain[dom_ord],
#'                       shape_id = shape_austria_dis$BKZ)
#'
#' # Create map plot for mean indicator - point and CV estimates but no MSE
#' # using the numerical domain identifiers of the shape file
#' map_plot(object = NER_model, MSE = FALSE, CV = TRUE,
#'          map_obj = shape_austria_dis,
#'          map_dom_id = "BKZ", map_tab = map_tab)
#' }
#'
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 aes geom_sf facet_wrap coord_equal labs
#' @importFrom ggplot2 theme element_blank scale_fill_gradient ggplot ggtitle
#' @importFrom rlang .data

map_plot <- function(object,
                     MSE = FALSE,
                     CV = FALSE,
                     map_obj = NULL,
                     map_dom_id = NULL,
                     map_tab = NULL,
                     color = c("white", "red4"),
                     scale_points = NULL,
                     guide = "colourbar",
                     return_data = FALSE
                     ) {

  indicator <- c("Mean")

  if (is.null(map_obj)) {

    message("No Map Object has been provided. An artificial polygone is used for
             visualization")

    map_pseudo(object    = object,
               indicator = indicator,
               panelplot = FALSE,
               MSE       = MSE,
               CV        = CV
    )
  } else if (!inherits(map_obj, "sf")) {

    stop("map_obj is not of class sf from the sf package")

  } else {

    if (length(color) != 2 || !is.vector(color)) {
      stop(paste("col needs to be a vector of length 2 defining the starting,",
                 "mid and upper color of the map-plot"))
    }

    plot_real(object       = object,
              indicator    = indicator,
              MSE          = MSE,
              CV           = CV,
              map_obj      = map_obj,
              map_dom_id   = map_dom_id,
              map_tab      = map_tab,
              col          = color,
              scale_points = scale_points,
              return_data  = return_data,
              guide        = guide
    )
  }
}

map_pseudo <- function(object, indicator, panelplot, MSE, CV) {

  x <- y <- id <- value <- NULL

  values <-  estimators(object    = object,
                        indicator = indicator,
                        MSE       = MSE,
                        CV        = CV
  )$ind

  indicator <- colnames(values)[-1]

  tplot <- get_polygone(values = values)

  if (panelplot) {
    ggplot(tplot, aes(x = x, y = y)) +
      geom_sf(aes(group = id, fill = value)) +
      facet_wrap(facets = ~ variable,
                 ncol   = ceiling(sqrt(length(unique(tplot$variable))))
      )
  } else {
    for (ind in indicator) {
      print(ggplot(tplot[tplot$variable == ind, ], aes(x = x, y = y)) +
              ggtitle(paste0(ind)) +
              geom_sf(aes(group = id, fill = value)))
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
}

plot_real <- function(object,
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      map_obj = NULL,
                      map_dom_id = NULL,
                      map_tab = NULL,
                      col = col,
                      scale_points = NULL,
                      return_data = FALSE,
                      guide = NULL) {


  if (!is.null(map_obj) && is.null(map_dom_id)) {
    stop("No Domain ID for the map object is given")
  }

  long <- lat <- group <- NULL

  map_data <- estimators(object    = object,
                         indicator = indicator,
                         MSE       = MSE,
                         CV        = CV
  )$ind

  if (!is.null(map_tab)) {
    map_data <- merge(x    = map_data,
                      y    = map_tab,
                      by.x = "Domain",
                      by.y = names(map_tab)[1]
    )
    matcher <- match(map_obj[[map_dom_id]],
                     map_data[, names(map_tab)[2]])

    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("Domains of map_tab and Map object do not match. Check map_tab")
      } else {
        warnings(paste("Not all Domains of map_tab and Map object could be",
                       "matched. Check map_tab"))
      }
    }

    map_data <- map_data[matcher, ]
    map_data <- map_data[, !colnames(map_data) %in%
                           c("Domain", map_dom_id), drop = F]
    map_data$Domain <- map_data[, colnames(map_data) %in% names(map_tab)]
  } else {
    matcher <- match(map_obj[[map_dom_id]], map_data[, "Domain"])

    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop(paste("Domain of saeTrafo object and Map object do not match.",
                   "Try using map_tab"))
      } else {
        warnings(paste("Not all Domains of saeTrafo object and Map object",
                       "could be matched. Try using map_tab"))
      }
    }
    map_data <- map_data[matcher, ]
  }

  map_obj.merged <- merge(map_obj, map_data, by.x = map_dom_id, by.y = "Domain")

  indicator <- colnames(map_data)
  indicator <- indicator[!(indicator %in% "Domain")]

  for (ind in indicator) {

    map_obj.merged[[ind]][!is.finite(map_obj.merged[[ind]])] <- NA

    scale_point <- get_scale_points(y            = map_obj.merged[[ind]],
                                    ind          = ind,
                                    scale_points = scale_points
    )

    print(ggplot(data = map_obj.merged,
                 aes(long, lat, group = group, fill = .data[[ind]])) +
            geom_sf(color = "azure3") +
            labs(x = "", y = "", fill = ind) +
            ggtitle(gsub(pattern = "_", replacement = " ", x = ind)) +
            scale_fill_gradient(low    = col[1],
                                high   = col[2],
                                limits = scale_point,
                                guide  = guide
            ) +
            theme(axis.ticks   = element_blank(),
                  axis.text    = element_blank(),
                  legend.title = element_blank()
            )
    )

    if (!ind == tail(indicator, 1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  if (return_data) {
    return(map_obj.merged)
  }
}

get_polygone <- function(values) {

  if (is.null(dim(values))) {
    values <- as.data.frame(values)
  }

  n <- nrow(values)
  cols <- ceiling(sqrt(n))
  n <- cols^2

  values["id"] <- seq_len(nrow(values))

  poly <- data.frame(id       = rep(seq_len(n), each = 4),
                     ordering = seq_len((n * 4)),
                     x        = c(0, 1, 1, 0) +
                       rep(0:(cols - 1), each = (cols * 4)),
                     y        = rep(c(0, 0, 1, 1) +
                                      rep(0:(cols - 1), each = 4), cols)
  )

  combo <- merge(poly, values, by = "id", all = TRUE, sort = FALSE)

  melt(data    = combo[order(combo$ordering), ],
       id.vars = c("id", "x", "y", "ordering")
  )
}

get_scale_points <- function(y, ind, scale_points) {

  result <- NULL

  if (!is.null(scale_points)) {
    if (inherits(scale_points, "numeric") && length(scale_points) == 2) {
      result <- scale_points
    }
  }
  if (is.null(result)) {
    rg <- range(y, na.rm = TRUE)
    result <- rg
  }
  return(result)
}
