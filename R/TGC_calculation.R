#' Calculate the Thermal Growth Coefficent.
#'
#' Calculates the Thermal Growth Coefficent (TGC) based on initial weight (grams), final weight (grams), number of days between the two weight measurements and the temperature (degrees C).
#' @param startWeight numeric value, weight (g) at first measurement
#' @param finalWeight numeric value, weight (g) at second measurement
#' @param days numeric value, days between the two measurements
#' @param degrees numeric value, average temperature of the water
#' @export
#' @examples
#' TGC_calculation(1.7, 24.8, 65, 12)

TGC_calculation <- function(startWeight, finalWeight, days, degrees = 12){
    1000*(finalWeight^(1/3)-startWeight^(1/3))*(1/(degrees*days))
}

