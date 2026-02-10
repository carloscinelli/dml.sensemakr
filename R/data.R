#' @title Net financial assets and 401(k) eligibility
#'
#' @description
#' Data on net financial assets and 401(k) eligibility.
#'
#' @docType data
#'
#' @usage data('pension')
#' @format A data.frame with 9915 observations and 44 variables, among them:
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

#' @title Pennsylvania Reemployment Bonus Experiment
#'
#' @description
#' Data from the Pennsylvania Reemployment Bonus experiment. The experiment randomly assigned
#' unemployment insurance claimants to a treatment group that received a cash bonus for finding
#' employment quickly.
#'
#' @docType data
#'
#' @usage data('Penn')
#' @format A data.frame with 5099 observations and 23 variables, among them:
#' \itemize{
#'  \item \strong{tg:} =1 if assigned to the treatment group (reemployment bonus)
#'  \item \strong{inuidur1:} log unemployment duration
#'  \item \strong{inuidur2:} unemployment duration in weeks
#'  \item \strong{female:} =1 if female
#'  \item \strong{black:} =1 if Black
#'  \item \strong{hispanic:} =1 if Hispanic
#'  \item \strong{othrace:} =1 if other race
#'  \item \strong{dep:} number of dependents
#'  \item \strong{q1-q6:} quarter of entry indicators
#'  \item \strong{recall:} =1 if expecting recall
#'  \item \strong{agelt35:} =1 if age less than 35
#'  \item \strong{agegt54:} =1 if age greater than 54
#'  \item \strong{durable:} =1 if durable manufacturing industry
#'  \item \strong{nondurable:} =1 if nondurable manufacturing industry
#'  \item \strong{lusd:} =1 if low unemployment state duration
#'  \item \strong{husd:} =1 if high unemployment state duration
#'  \item \strong{muld:} =1 if mid unemployment state duration
#' }
#'
#' @examples
#' data('Penn')
#' head(Penn)
"Penn"

#' @title Household gasoline demand
#'
#' @description
#' Data on household gasoline demand with price, income, and geographic characteristics.
#'
#' @docType data
#'
#' @usage data('gasdemand')
#' @format A data.frame with 3640 observations and 23 variables, among them:
#' \itemize{
#'  \item \strong{log_q:} log gasoline quantity demanded
#'  \item \strong{log_p:} log gasoline price
#'  \item \strong{log_y:} log household income
#'  \item \strong{log_driver:} log number of drivers in household
#'  \item \strong{log_hhr_age:} log household head age
#'  \item \strong{log_hhsize:} log household size
#'  \item \strong{total_wrkr:} total number of workers
#'  \item \strong{publictransit_d:} =1 if public transit available
#'  \item \strong{distance_oil1000:} distance to nearest oil refinery (in 1000s)
#'  \item \strong{cl5_secondcity_d:} =1 if in a second city
#'  \item \strong{cl5_smtown_d:} =1 if in a small town
#'  \item \strong{cl5_suburban_d:} =1 if suburban area
#'  \item \strong{cl5_urban_d:} =1 if urban area
#'  \item \strong{popdensity_d2-d8:} population density indicators
#'  \item \strong{share:} survey weight
#'  \item \strong{state_fips:} state FIPS code
#'  \item \strong{region:} region code
#' }
#'
#' @examples
#' data('gasdemand')
#' head(gasdemand)
"gasdemand"
