#' Fetch data from the original source
#'
#' This function fetches from the main dataset, keeps variables and observations
#' relevant to this specific analysis project, restricts the sample size as
#' needed, and lastly saves the new dataset as an `.rda` file in the `data/`
#' folder. The newly created dataset can be accessed anywhere while in the
#' RStudio project using `devtools::load_all()`.
#'
#' @return Saves the wrangled data into the data/ folder.
#'
#' @examples
#' # Run this command in the console to create the data file.
#' fetch_data()
#'
fetch_data <- function() {
    # Load the main dataset and wrangle as necessary.
    project_data <- PROMISE.data::PROMISE %>%

    # Merge PROMISE with other PROMISE data sets
    PROMISE.data::combine_datasets(meds, bloodwork, ogtt,
                                   misc$dm, misc$months, misc$phys_act) %>%

    # Data wrangling commands
      dplyr::filter(is.na(Canoe)) %>% # exclude subjects from Canoe
      dplyr::mutate(
        UDBP = ifelse(UDBP == 0.01, NA, UDBP),
        UrineCreatinine = ifelse(SID == 2028, 9, UrineCreatinine), # error with data entry
        ACR = round(UrineMicroalbumin / UrineCreatinine, digits = 2),
        Ethnicity = as.character(Ethnicity),
        isAfrican = ifelse(Ethnicity == 'African', 1, 0),
        Ethnicity = ifelse(
          Ethnicity %in% c('African', 'First Nations', 'Other'),
          'Other',
          Ethnicity
        ),
        fVN = factor(
          VN,
          levels = c(1, 3, 6),
          labels = c("Baseline", "3Year", "6Year"),
          ordered = TRUE
        ),
        YearsFromBaseline = MonthsFromBaseline/12,
        fMedsBP = factor(
          MedsBloodPressure,
          levels = c(0, 1),
          labels = c("No", "Yes")
        ),
        Hypertension = ifelse(Systolic > 140 | Diastolic > 90, 1,
                              0),
        Hypertension = factor(Hypertension,
                              levels = c(0, 1),
                              labels = c("No", "Yes")),
        dmStatus = ifelse(DM == 1, "DM",
                          ifelse(IFG == 1 |
                                   IGT == 1, "PreDM",
                                 "NGT")),
        dmStatus = ifelse(is.na(dmStatus), "NGT", dmStatus),
        dmStatus = factor(dmStatus,
                          levels = c("NGT", "PreDM", "DM"),
                          ordered = TRUE),
        acrStatus = ifelse(
          ACR < 2,
          'Normoalbuminuria',
          ifelse(ACR > 20, 'Macroalbuminuria',
                 "Microalbuminuria")
        ),
        acrStatus = factor(
          acrStatus,
          levels = c("Normoalbuminuria", "Microalbuminuria", "Macroalbuminuria"),
          ordered = TRUE
        ),
        creat.mgdl = Creatinine * 0.011312, # conversion of creatinine units for eGFR calculation
        eGFR = nephro::CKDEpi.creat(creat.mgdl, as.numeric(Sex) -
                                      1, Age, isAfrican),
        eGFR_mdrd = nephro::MDRD4(creat.mgdl, as.numeric(Sex) - 1,
                                  Age, isAfrican),
        eGFRStatus = cut(eGFR,
                         breaks = c(-Inf, 60, 90, Inf),
                         labels = c("Moderate", "Mild", "Normal")),
        eGFRStatus = factor(eGFRStatus,
                            levels = c("Normal", "Mild", "Moderate"),
                            ordered = TRUE),
        udbpStatus = cut(UDBP,
                         breaks = c(0, 1.23, 60, Inf),
                         labels = c("Trace", "Normal", "High"),
                         ordered_result = TRUE),
        udbpTertile = ntile(UDBP, 3),
        udbpCr = UDBP / UrineCreatinine,
        logudbpCr = log(udbpCr),
        udbpCrTertile = ntile(udbpCr, 3),
        vitdStatus = cut(VitaminD,
                         breaks = c(-Inf, 50, 75, Inf),
                         labels = c("Deficient", "Insufficient", "Sufficient"),
                         ordered = TRUE),
        Season = ifelse(lubridate::month(VisitDate) %in% c("5", "6", "7", "8", "9", "10"),
                        "Summer", "Winter")
      ) %>%
      dplyr::filter(UDBP < 10000) %>%
      dplyr::filter(eGFR < 200) %>%
      dplyr::filter(eGFR_mdrd < 300) %>%
      dplyr::filter(Creatinine < 200) %>%
      dplyr::select(
        SID,
        VN,
        fVN,
        MonthsFromBaseline,
        YearsFromBaseline,
        VisitDate,
        BMI,
        Waist,
        Age,
        Sex,
        Ethnicity,
        Glucose0,
        Glucose120,
        Dysgly,
        DM,
        IFG,
        IGT,
        dmStatus,
        acrStatus,
        eGFRStatus,
        udbpStatus,
        udbpTertile,
        udbpCrTertile,
        eGFR,
        eGFR_mdrd,
        ACR,
        UrineMicroalbumin,
        UrineCreatinine,
        Creatinine,
        UDBP,
        udbpCr,
        logudbpCr,
        VitaminD,
        vitdStatus,
        Season,
        MeanArtPressure,
        Systolic,
        Diastolic,
        Hypertension,
        PTH,
        ALT,
        fMedsBP,
        MET,
        CRP,
        MedsBloodPressure
        # dplyr::matches("meds")
      ) %>%

    # Save the dataset to the data/ folder.
    # save(file = "data/project_data.rda")
    devtools::use_data(project_data, overwrite = TRUE)
}
