#' @title Net financial assets and 401(k) eligibility
#'
#' @description
#' Data on net financial assets and 401(k) eligibility.
#'
#' @docType data
#'
#' @usage data('pension')
#' @format A data.frame with 3010 observations and 34 variables:
#' \itemize{
#'  \item \strong{net_tfa:} net total financial assets
#'  \item \strong{e401:} =1 if employer offers 401(k)
#'  \item \strong{p401:} =1 if individual participates in a 401(k) plan
#'  \item \strong{age:} age
#'  \item \strong{inc:} income
#'  \item \strong{fsize:} family size
#'  \item \strong{educ:} years of education
#'  \item \strong{db:} =1 if individual has defined benefit pension
#'  \item \strong{marr:} =1 if married
#'  \item \strong{twoearn:} =1 if two-earner household
#'  \item \strong{pira:} =1 if individual participates in IRA plan
#'  \item \strong{hown:} =1 if home owner
#' }
#'
#' @examples
#' data('pension')
#' head(pension)
"pension"
