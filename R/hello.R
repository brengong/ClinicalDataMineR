

#' loaddata
#'
#' loads the data for use in further analyses.
#'
#' @param  fname a csv file to load.
#' @return A loaded data.table
#' @author Brendan Gongol
#' @importFrom data.table fread
#' @importFrom data.table data.table
#' @importFrom dtplyr tbl_dt
#' @export

loaddata <- function(fname)
{
  where = c("OR","ICU","neuro","surgeries","ER")
  readin <- fread(fname) %>% tbl_dt

  nmes <- names(readin)
  nmes <- gsub('[" ","+","-","/","_"]',"", tolower(nmes))
  nmes <- gsub("-","",nmes)

  setnames(readin,nmes)
  slug <- as.integer(strsplit(fname," ")[[1]][1])
  readin$wherefrom <- where[slug]

  return(readin)
}






#' frequenciesdyn
#'
#' Computes the frequencies of a particular attribute
#'
#' @param  DTstr the objest of a data table entered in quotations (eg: "ulcer")
#' @param  xstr a column in a data table entered in quotations (eg: "patmrnid")
#' @return frequency counts of xstr in DTstr
#' @author Brendan Gongol
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom data.table data.table
#' @export

frequenciesdyn <- function(DTstr, xstr)
{
  return(eval(parse(text=sprintf('%s[,.(count=.N),.(%s)]', DTstr, xstr))) %>%
           arrange(desc(count)) %>% mutate(percent=100*count/sum(count)))
}




#' EmptyColumnRemoveR
#'
#' Removes the empty columns from a data table.
#'
#' @param  DT the data table to remove the empty columns from.
#' @return a data table containing conly columns that have at least one entry.
#' @author Brendan Gongol
#' @export

EmptyColumnRemoveR <- function(DT){
  DTcomplete <- data.table(1:nrow(DT))
  for(i in 1:ncol(DT)){
    if((sum(is.na(DT[,i, with = FALSE])) == nrow(DT)) == FALSE){
      contains <-   DT[,i, with = FALSE]
      DTcomplete <- cbind(DTcomplete, contains)
    }
  }
  DTcomplete$V1 <- NULL
  return(DTcomplete)
}




#' EmptyColumnRemoveR
#'
#' Removes the empty columns from a data table.
#'
#' @param  DT a data table containing a column labeled "pressureulcerstaging".
#' @return a data table containing the pressure ulcer stages according to record number
#' @author Brendan Gongol
#' @export

StageCleanR <- function(DT){
  PD <- DT$pressureulcerstaging
  PD <- gsub("Stage 1", "Stage I", PD)
  PD <- gsub("Stage 2", "Stage II", PD)
  PD <- gsub("Stage 3", "Stage III", PD)
  PD <- gsub("Stage 4", "Stage IV", PD)
  PD <- gsub("Suspected Deep Tissue Injury", "Deep Tissue Injury", PD)

  proc <- NULL
  for(i in 1:length(PD)){
    SP <- strsplit(PD[i], split = ",")
    SP <- trimws(SP[[1]])

    if(length(SP) > 0){
      dt <- data.table(SP, rep(i, length(SP)))
      setnames(dt, c("SP", "V2"), c("pressureulcerstaging", "recno"))
    }else{
      dt <- data.table(NA, i)
      setnames(dt, c("V1", "i"), c("pressureulcerstaging", "recno"))
    }
    proc <- rbind(proc, dt)

  }
  return(proc)
}



#' DiagnosisCleaner
#'
#' separates out the diagnosis code from the diagnosis annotation.
#'
#' @param  DT a data table containing a column named "patientdiagnosis" and a column named "recno".
#' @return a data table containing one column labeled "diagnoses", a second colum labeled "annotation" containing the diagnosis annotation, and a third column labeled "recno" containing the initial record number the diagnosis originated from.
#' @author Brendan Gongol
#' @export

DiagnosisCleaner <- function(DT){

  PD <- DT$`patientdiagnosis`

  PD <- gsub(", unspecified organism", "", PD, fixed=TRUE)
  PD <- gsub(", unspecified", "", PD, fixed=TRUE)
  PD <- gsub(", cigarettes" , "", PD, fixed=TRUE)
  PD <- gsub(", uncomplicated", "", PD, fixed=TRUE)
  PD <- gsub(", without rupture", "", PD, fixed=TRUE)
  PD <- gsub(", unilateral", "", PD, fixed=TRUE)
  PD <- gsub(", not elsewhere classified", "", PD, fixed=TRUE)
  PD <- gsub(", extent unspecified", "", PD, fixed=TRUE)
  PD <- gsub(", stage 2", "", PD, fixed=TRUE)
  PD <- gsub(", initial encounter", "", PD, fixed=TRUE)
  PD <- gsub(", adult", "", PD, fixed=TRUE)
  PD <- gsub(", implants and grafts", "", PD, fixed=TRUE)
  PD <- gsub(", stage 3 (moderate)", "", PD, fixed=TRUE)
  PD <- gsub(", single episode", "", PD, fixed=TRUE)
  PD <- gsub(", intraventricular", "", PD, fixed=TRUE)
  PD <- gsub(", site", "", PD, fixed=TRUE)
  PD <- gsub(", intractable", "", PD, fixed=TRUE)
  PD <- gsub(", without", "", PD, fixed=TRUE)
  PD <- gsub(", right", "", PD, fixed=TRUE)
  PD <- gsub(", not", "", PD, fixed=TRUE)
  PD <- gsub(", medicaments", "", PD, fixed=TRUE)
  PD <- gsub(", left", "", PD, fixed=TRUE)
  PD <- gsub(", or", "", PD, fixed=TRUE)
  PD <- gsub(", stage", "", PD, fixed=TRUE)
  PD <- gsub(", accidental", "", PD, fixed=TRUE)
  PD <- gsub(", except", "", PD, fixed=TRUE)
  PD <- gsub(", cause", "", PD, fixed=TRUE)
  PD <- gsub(", recurrent", "", PD, fixed=TRUE)
  PD <- gsub(", first", "", PD, fixed=TRUE)
  PD <- gsub(", and", "", PD, fixed=TRUE)
  PD <- gsub(", second", "", PD, fixed=TRUE)
  PD <- gsub(", subsequent", "", PD, fixed=TRUE)
  PD <- gsub(", bilateral", "", PD, fixed=TRUE)
  PD <- gsub(", cervical", "", PD, fixed=TRUE)
  PD <- gsub(", sequela", "", PD, fixed=TRUE)
  PD <- gsub(", other", "", PD, fixed=TRUE)
  PD <- gsub(", group", "", PD, fixed=TRUE)
  PD <- gsub(", with", "", PD, fixed=TRUE)
  PD <- gsub(", medicament", "", PD, fixed=TRUE)
  PD <- gsub(", both", "", PD, fixed=TRUE)
  PD <- gsub(", unstageable", "", PD, fixed=TRUE)
  PD <- gsub(", lumbar", "", PD, fixed=TRUE)
  PD <- gsub(", pharyngeal", "", PD, fixed=TRUE)
  PD <- gsub(", mass", "", PD, fixed=TRUE)
  PD <- gsub(", head", "", PD, fixed=TRUE)
  PD <- gsub(", hypnotic", "", PD, fixed=TRUE)
  PD <- gsub(", complete", "", PD, fixed=TRUE)
  PD <- gsub(", nutritional", "", PD, fixed=TRUE)
  PD <- gsub(", childbirth", "", PD, fixed=TRUE)
  PD <- gsub(", poisoning", "", PD, fixed=TRUE)
  PD <- gsub(", eyes", "", PD, fixed=TRUE)
  PD <- gsub(", never", "", PD, fixed=TRUE)
  PD <- gsub(", at", "", PD, fixed=TRUE)
  PD <- gsub(", best", "", PD, fixed=TRUE)
  PD <- gsub(", incomprehensible", "", PD, fixed=TRUE)
  PD <- gsub(", localizes", "", PD, fixed=TRUE)
  PD <- gsub(", cortical", "", PD, fixed=TRUE)
  PD <- gsub(", aortic", "", PD, fixed=TRUE)
  PD <- gsub(", complete", "", PD, fixed=TRUE)
  PD <- gsub(", valve", "", PD, fixed=TRUE)
  PD <- gsub(", acquired", "", PD, fixed=TRUE)
  PD <- gsub(", nonruptured", "", PD, fixed=TRUE)
  PD <- gsub(", part", "", PD, fixed=TRUE)
  PD <- gsub(", normal", "", PD, fixed=TRUE)
  PD <- gsub(", face", "", PD, fixed=TRUE)
  PD <- gsub(", thoracolumbar", "", PD, fixed=TRUE)
  PD <- gsub(", chewing", "", PD, fixed=TRUE)
  PD <- gsub(", fascia", "", PD, fixed=TRUE)
  PD <- gsub(", subcortical", "", PD, fixed=TRUE)
  PD <- gsub(", to", "", PD, fixed=TRUE)
  PD <- gsub(", none", "", PD, fixed=TRUE)
  PD <- gsub(", obeys", "", PD, fixed=TRUE)
  PD <- gsub(", in", "", PD, fixed=TRUE)
  PD <- gsub(", affecting", "", PD, fixed=TRUE)
  PD <- gsub(", malignant", "", PD, fixed=TRUE)
  PD <- gsub(", presenting", "", PD, fixed=TRUE)
  PD <- gsub(", lower", "", PD, fixed=TRUE)
  PD <- gsub(", asr", "", PD, fixed=TRUE)
  PD <- gsub(", mixed", "", PD, fixed=TRUE)
  PD <- gsub(", subendocardial", "", PD, fixed=TRUE)
  PD <- gsub(", status", "", PD, fixed=TRUE)
  PD <- gsub(", NOS", "", PD, fixed=TRUE)
  PD <- gsub(", pharyngoesophageal", "", PD, fixed=TRUE)
  PD <- gsub(", C5-C7complete","", PD, fixed=TRUE)
  PD <- gsub(", sixth", "", PD, fixed=TRUE)
  PD <- gsub(", seventh", "", PD, fixed=TRUE)
  PD <- gsub(", native", "", PD, fixed=TRUE)
  PD <- gsub(", continuous", "", PD, fixed=TRUE)
  PD <- gsub(", as", "", PD, fixed=TRUE)
  PD <- gsub(", great", "", PD, fixed=TRUE)
  PD <- gsub(", idiopathic", "", PD, fixed=TRUE)
  PD <- gsub(", eructation","", PD, fixed=TRUE)
  PD <- gsub(", most", "", PD, fixed=TRUE)
  PD <- gsub(", subdural", "", PD, fixed=TRUE)
  PD <- gsub(", loss", "", PD, fixed=TRUE)
  PD <- gsub(", prolonged","", PD, fixed=TRUE)
  PD <- gsub(", below", "", PD, fixed=TRUE)
  PD <- gsub(", complicated", "", PD, fixed=TRUE)
  PD <- gsub(", implant", "", PD, fixed=TRUE)
  PD <- gsub(", late", "", PD, fixed=TRUE)
  PD <- gsub(", uncontrolled", "", PD, fixed=TRUE)
  PD <- gsub(", shoulder", "", PD, fixed=TRUE)
  PD <- gsub(", no", "", PD, fixed=TRUE)
  PD <- gsub(", antibodies", "", PD, fixed=TRUE)
  PD <- gsub(", benign", "", PD, fixed=TRUE)
  PD <- gsub(", forearm", "", PD, fixed=TRUE)
  PD <- gsub(", type", "", PD, fixed=TRUE)
  PD <- gsub(", current", "", PD, fixed=TRUE)
  PD <- gsub(", genital", "", PD, fixed=TRUE)
  PD <- gsub(", perforation", "", PD, fixed=TRUE)
  PD <- gsub(", cardiogenic", "", PD, fixed=TRUE)
  PD <- gsub(", ruptured", "", PD, fixed=TRUE)
  PD <- gsub(", following", "", PD, fixed=TRUE)
  PD <- gsub(", primary", "", PD, fixed=TRUE)
  PD <- gsub(", myelitis", "", PD, fixed=TRUE)
  PD <- gsub(", pediatric","", PD, fixed=TRUE)
  PD <- gsub(", 5th", "", PD, fixed=TRUE)
  PD <- gsub(", anterior", "", PD, fixed=TRUE)
  PD <- gsub(", excluding", "", PD, fixed=TRUE)
  PD <- gsub(", moderate", "", PD, fixed=TRUE)
  PD <- gsub(", minor", "", PD, fixed=TRUE)
  PD <- gsub(", multiple", "", PD, fixed=TRUE)
  PD <- gsub(", chronic", "", PD, fixed=TRUE)
  PD <- gsub(", peritoneum", "", PD, fixed=TRUE)
  PD <- gsub(", tubercle", "", PD, fixed=TRUE)
  PD <- gsub(", but", "", PD, fixed=TRUE)
  PD <- gsub(", extranodal", "", PD, fixed=TRUE)
  PD <- gsub(", closed", "", PD, fixed=TRUE)
  PD <- gsub(", autosomal", "", PD, fixed=TRUE)
  PD <- gsub(", tendon", "", PD, fixed=TRUE)
  PD <- gsub(", septic", "", PD, fixed=TRUE)
  PD <- gsub(", pelvic", "", PD, fixed=TRUE)
  PD <- gsub(", localized", "", PD, fixed=TRUE)
  PD <- gsub(", ankle", "", PD, fixed=TRUE)
  PD <- gsub(", ligament", "", PD, fixed=TRUE)
  PD <- gsub(", leg","", PD, fixed=TRUE)
  PD <- gsub(", specified", "", PD, fixed=TRUE)
  PD <- gsub(", brief", "", PD, fixed=TRUE)
  PD <- gsub(", abdominal", "", PD, fixed=TRUE)
  PD <- gsub(", neck", "", PD, fixed=TRUE)
  PD <- gsub(", abrasion", "", PD, fixed=TRUE)
  PD <- gsub(", sacrum", "", PD, fixed=TRUE)
  PD <- gsub(", bronchus", "", PD, fixed=TRUE)
  PD <- gsub(", middle", "", PD, fixed=TRUE)
  PD <- gsub(", upper", "", PD, fixed=TRUE)
  PD <- gsub(", thigh", "", PD, fixed=TRUE)
  PD <- gsub(", state", "", PD, fixed=TRUE)
  PD <- gsub(", scalp", "", PD, fixed=TRUE)
  PD <- gsub(", due", "", PD, fixed=TRUE)
  PD <- gsub(", distal", "", PD, fixed=TRUE)
  PD <- gsub(", rectosigmoid", "", PD, fixed=TRUE)
  PD <- gsub(", spontaneous", "", PD, fixed=TRUE)
  PD <- gsub(", flexion", "", PD, fixed=TRUE)
  PD <- gsub(", benzothiadiazides", "", PD, fixed=TRUE)
  PD <- gsub(", lymph", "", PD, fixed=TRUE)
  PD <- gsub(", dysarthria", "", PD, fixed=TRUE)
  PD <- gsub(", one", "", PD, fixed=TRUE)
  PD <- gsub(", impairment", "", PD, fixed=TRUE)
  PD <- gsub(", hand", "", PD, fixed=TRUE)
  PD <- gsub(", epigastric", "", PD, fixed=TRUE)
  PD <- gsub(", generalized", "", PD, fixed=TRUE)
  PD <- gsub(", above", "", PD, fixed=TRUE)
  PD <- gsub(", soft", "", PD, fixed=TRUE)
  PD <- gsub(", medicinal", "", PD, fixed=TRUE)
  PD <- gsub(", foot", "", PD, fixed=TRUE)
  PD <- gsub(", blister", "", PD, fixed=TRUE)
  PD <- gsub(", acute", "", PD, fixed=TRUE)
  PD <- gsub(", stomach", "", PD, fixed=TRUE)
  PD <- gsub(", open", "", PD, fixed=TRUE)
  PD <- gsub(", mild", "", PD, fixed=TRUE)
  PD <- gsub(", sacral", "", PD, fixed=TRUE)
  PD <- gsub(", extension","", PD, fixed=TRUE)
  PD <- gsub(", bipolar", "", PD, fixed=TRUE)
  PD <- gsub(", irregular", "", PD, fixed=TRUE)
  PD <- gsub(", plaque", "", PD, fixed=TRUE)
  PD <- gsub(", so", "", PD, fixed=TRUE)
  PD <- gsub(", low", "", PD, fixed=TRUE)
  PD <- gsub(", hematoma", "", PD, fixed=TRUE)
  PD <- gsub(", result", "", PD, fixed=TRUE)
  PD <- gsub("buttock(707.05)", "707.05", PD, fixed=TRUE)
  PD <- gsub(", vertebra(e)", "", PD, fixed=TRUE)
  PD <- gsub("hip(707.04)", "707.04", PD, fixed=TRUE)
  PD <- gsub("uncontrolled(250.12)", "250.12", PD, fixed=TRUE)
  PD <- gsub("elbow(707.01)", "707.01", PD, fixed=TRUE)
  PD <- gsub("heel(707.07)", "707.07", PD, fixed=TRUE)
  PD <- gsub("uncontrolled(250.72)", "250.72", PD, fixed=TRUE)
  PD <- gsub("ankle(707.06)", "707.06", PD, fixed=TRUE)
  PD <- gsub("(not", "not", PD, fixed=TRUE)
  PD <- gsub("recurrent)", "recurrent", PD, fixed=TRUE)
  PD <- gsub("or,", "or", PD, fixed=TRUE)
  PD <- gsub("unspecified, ", "unspecified", PD, fixed=TRUE)
  #### Fix unspecified issue
  PD <- gsub("unspecifiedK62.5", "unspecified, K62.5", PD, fixed=TRUE)
  PD <- gsub("unspecified496", "unspecified, 496", PD, fixed=TRUE)
  PD <- gsub("unspecified780.01", "unspecified, 780.01", PD, fixed=TRUE)
  PD <- gsub("unspecifiedJ90", "unspecified, J90", PD, fixed=TRUE)
  PD <- gsub("unspecified378.83", "unspecified, 378.83", PD, fixed=TRUE)
  PD <- gsub("unspecified853.06", "unspecified, 853.06", PD, fixed=TRUE)
  PD <- gsub("unspecified0", "unspecified, 0", PD, fixed=TRUE)
  PD <- gsub("unspecified1", "unspecified, 1", PD, fixed=TRUE)
  PD <- gsub("unspecified2", "unspecified, 2", PD, fixed=TRUE)
  PD <- gsub("unspecified3", "unspecified, 3", PD, fixed=TRUE)
  PD <- gsub("unspecified4", "unspecified, 4", PD, fixed=TRUE)
  PD <- gsub("unspecified5", "unspecified, 5", PD, fixed=TRUE)
  PD <- gsub("unspecified6", "unspecified, 6", PD, fixed=TRUE)
  PD <- gsub("unspecified7", "unspecified, 7", PD, fixed=TRUE)
  PD <- gsub("unspecified8", "unspecified, 8", PD, fixed=TRUE)
  PD <- gsub("unspecified9", "unspecified, 9", PD, fixed=TRUE)
  PD <- gsub("unspecifiedA", "unspecified, A", PD, fixed=TRUE)
  PD <- gsub("unspecifiedB", "unspecified, B", PD, fixed=TRUE)
  PD <- gsub("unspecifiedC", "unspecified, C", PD, fixed=TRUE)
  PD <- gsub("unspecifiedD", "unspecified, D", PD, fixed=TRUE)
  PD <- gsub("unspecifiedE", "unspecified, E", PD, fixed=TRUE)
  PD <- gsub("unspecifiedF", "unspecified, F", PD, fixed=TRUE)
  PD <- gsub("unspecifiedG", "unspecified, G", PD, fixed=TRUE)
  PD <- gsub("unspecifiedH", "unspecified, H", PD, fixed=TRUE)
  PD <- gsub("unspecifiedI", "unspecified, I", PD, fixed=TRUE)
  PD <- gsub("unspecifiedJ", "unspecified, J", PD, fixed=TRUE)
  PD <- gsub("unspecifiedK", "unspecified, K", PD, fixed=TRUE)
  PD <- gsub("unspecifiedL", "unspecified, L", PD, fixed=TRUE)
  PD <- gsub("unspecifiedM", "unspecified, M", PD, fixed=TRUE)
  PD <- gsub("unspecifiedN", "unspecified, N", PD, fixed=TRUE)
  PD <- gsub("unspecifiedO", "unspecified, O", PD, fixed=TRUE)
  PD <- gsub("unspecifiedP", "unspecified, P", PD, fixed=TRUE)
  PD <- gsub("unspecifiedQ", "unspecified, Q", PD, fixed=TRUE)
  PD <- gsub("unspecifiedR", "unspecified, R", PD, fixed=TRUE)
  PD <- gsub("unspecifiedS", "unspecified, S", PD, fixed=TRUE)
  PD <- gsub("unspecifiedT", "unspecified, T", PD, fixed=TRUE)
  PD <- gsub("unspecifiedU", "unspecified, U", PD, fixed=TRUE)
  PD <- gsub("unspecifiedV", "unspecified, V", PD, fixed=TRUE)
  PD <- gsub("unspecifiedW", "unspecified, W", PD, fixed=TRUE)
  PD <- gsub("unspecifiedX", "unspecified, X", PD, fixed=TRUE)
  PD <- gsub("unspecifiedY", "unspecified, Y", PD, fixed=TRUE)
  PD <- gsub("unspecifiedZ", "unspecified, Z", PD, fixed=TRUE)
  ##############################################################
  #### Fix befavior issue
  PD <- gsub("behavior 682.2", "behavior, 682.2", PD, fixed=TRUE)
  PD <- gsub("behavior 298.9", "behavior, 298.9", PD, fixed=TRUE)
  PD <- gsub("behavior", "behavior,", PD, fixed=TRUE)
  ##############################################################
  PD <- gsub("tremor 345.90", "tremor, 345.90", PD, fixed=TRUE)
  PD <- gsub("respirator Z78.1", "respirator, Z78.1", PD, fixed=TRUE)
  PD <- gsub("tremor 401.9", "tremor, 401.9", PD, fixed=TRUE)
  PD <- gsub("unspecifiedR09.02", "unspecified, R09.02", PD, fixed=TRUE)
  PD <- gsub("unspecified584.9", "unspecified, 584.9", PD, fixed=TRUE)
  PD <- gsub("unspecifiedZ66", "unspecified, Z66", PD, fixed=TRUE)
  PD <- gsub("unspecifiedM54.30", "unspecified, M54.30", PD, fixed=TRUE)
  PD <- gsub(", thoracic", " thoracic", PD, fixed=TRUE)
  PD <- gsub("unspecified785.52 Septic shock(785.52)", ", 785.52 Septic shock(785.52)", PD, fixed=TRUE)
  PD <- gsub("unspecifiedM41.9 Scoliosis", ", M41.9 Scoliosis", PD, fixed=TRUE)
  PD <- gsub("Stridor 787.20", "Stridor, 787.20", PD, fixed=TRUE)
  PD <- gsub("tremor G57.91", "tremor, G57.91", PD, fixed=TRUE)
  PD <- gsub("unspecified995.91", "unspecified, 995.91", PD, fixed=TRUE)
  PD <- gsub(", C1-C4", " C1-C4", PD, fixed=TRUE)
  PD <- gsub(", C5-C7", " C5-C7", PD, fixed=TRUE)
  PD <- gsub("IIIA, IIIB, or IIIC", "IIIA IIIB or IIIC", PD, fixed=TRUE)
  PD <- gsub(", 24 hours or more after", " 24 hours or more after", PD, fixed=TRUE)
  PD <- gsub("organ, lung", " organ lung", PD, fixed=TRUE)
  PD <- gsub("IIIA, IIIB,", "IIIA IIIB", PD, fixed=TRUE)
  PD <- gsub("IIIA,", "IIIA", PD, fixed=TRUE)
  PD <- gsub("Major depressive disorder, single episode, severe, without mention of psychotic behavior", "Major depressive disorder single episode severe without mention of psychotic behavior", PD, fixed=TRUE)
  PD <- gsub("disorder, severe", "disorder severe", PD, fixed=TRUE)
  PD <- gsub("behavior 300.9", "behavior, 300.9", PD, fixed=TRUE)
  PD <- gsub("elsewhere, antepartum", "elsewhere antepartum", PD, fixed=TRUE)
  PD <- gsub("episode, severe", "episode severe", PD, fixed=TRUE)
  PD <- gsub("behavior 296.80", "behavior, 296.80", PD, fixed=TRUE)
  PD <- gsub("mother, antepartum", "mother antepartum", PD, fixed=TRUE)
  PD <- gsub("test, negative", "test negative", PD, fixed=TRUE)
  PD <- gsub("test, positive", "test positive", PD, fixed=TRUE)
  PD <- gsub("pregnancy, antepartum", "pregnancy antepartum", PD, fixed=TRUE)
  PD <- gsub("limbs, antepartum(648.73)", "limbs antepartum", PD, fixed=TRUE)
  PD <- gsub("antipsychotics, neuroleptics", "antipsychotics neuroleptics", PD, fixed=TRUE)
  PD <- gsub("complication, antepartum(646.83)", "complication antepartum", PD, fixed=TRUE)
  PD <- gsub("disease, antepartum(647.83)", "disease antepartum", PD, fixed=TRUE)
  PD <- gsub("mother, complicating", "mother complicating", PD, fixed=TRUE)
  PD <- gsub("Anemia, antepartum(648.23)", "Anemia antepartum", PD, fixed=TRUE)
  PD <- gsub("Thyroid dysfunction, antepartum(648.13)", "Thyroid dysfunction antepartum", PD, fixed=TRUE)
  PD <- gsub("maternal back, pelvis, lower limbs, antepartum(648.73)", "maternal back pelvis lower limbs antepartum", PD, fixed=TRUE)
  PD <- gsub("Diabetes mellitus, antepartum(648.03)", "Diabetes mellitus, antepartum(648.03)", PD, fixed=TRUE)
  PD <- gsub("radiculopathy, lumbosacral", "radiculopathy lumbosacral", PD, fixed=TRUE)
  PD <- gsub("atrium, auricular", "atrium auricular", PD, fixed=TRUE)
  PD <- gsub("amphetamines, undetermined", "amphetamines undetermined", PD, fixed=TRUE)
  PD <- gsub("ovary, fallopian", "ovary fallopian", PD, fixed=TRUE)
  PD <- gsub("lymphoid, hematopoietic", "lymphoid hematopoietic", PD, fixed=TRUE)
  PD <- gsub("infusion, transfusion", "infusion transfusion", PD, fixed=TRUE)
  PD <- gsub("aorta, thoracic", "aorta thoracic", PD, fixed=TRUE)
  PD <- gsub("infarction, episode", "infarction episode", PD, fixed=TRUE)
  PD <- gsub("strabismus, fourth", "strabismus fourth", PD, fixed=TRUE)
  PD <- gsub("Neuralgia, neuritis", "Neuralgia neuritis", PD, fixed=TRUE)
  PD <- gsub("tract, postpartum", "tract postpartum", PD, fixed=TRUE)
  PD <- gsub("disorder, postpartum(674.04)", "disorder postpartum", PD, fixed=TRUE)
  PD <- gsub("toe(s), superficial", "toe(s) superficial", PD, fixed=TRUE)
  PD <- gsub("strabismus, external", "strabismus external", PD, fixed=TRUE)
  PD <- gsub("enterocele, congenital", "enterocele congenital", PD, fixed=TRUE)
  PD <- gsub("adhesions, female", "adhesions female", PD, fixed=TRUE)
  PD <- gsub("Cystocele, midline", "Cystocele midline", PD, fixed=TRUE)
  PD <- gsub("drug, medical", "drug medical", PD, fixed=TRUE)
  PD <- gsub("ovary, ovarian", "ovary ovarian", PD, fixed=TRUE)
  PD <- gsub("gangrene, not", "gangrene not", PD, fixed=TRUE)
  PD <- gsub("laceration, major", "laceration major", PD, fixed=TRUE)
  PD <- gsub("deficiency, fibular", "deficiency fibular", PD, fixed=TRUE)
  PD <- gsub("Kyphosis, postlaminectomy", "Kyphosis postlaminectomy", PD, fixed=TRUE)
  PD <- gsub("pain, periumbilic", "pain periumbilic", PD, fixed=TRUE)
  PD <- gsub("delivery, delivered(660.21)", "delivery delivered(660.21)", PD, fixed=TRUE)
  PD <- gsub("delivery, delivered", "delivery delivered", PD, fixed=TRUE)
  PD <- gsub("delivery, single", "delivery single", PD, fixed=TRUE)
  PD <- gsub("Twin gestation, dichorionic/diamniotic (two placentae, two amniotic sacs)(V91.03)", "Twin gestation dichorionic/diamniotic (two placentae two amniotic sacs)", PD, fixed=TRUE)
  PD <- gsub("eyelid, full-thickness", "eyelid full-thickness", PD, fixed=TRUE)
  PD <- gsub("disproportion, delivered(653.41)", "disproportion delivered", PD, fixed=TRUE)
  PD <- gsub("delivery, delivered(660.11)", "delivery delivered", PD, fixed=TRUE)
  PD <- gsub("gestation, monochorionic/monoamniotic", "gestation monochorionic/monoamniotic", PD, fixed=TRUE)
  PD <- gsub("Anemia, postpartum(648.24)", "Anemia postpartum", PD, fixed=TRUE)
  PD <- gsub("mellitus, antepartum(648.03)", "mellitus antepartum", PD, fixed=TRUE)
  PD <- gsub("tolerance, complicating", "tolerance complicating", PD, fixed=TRUE)
  PD <- gsub("back, pelvis", "back pelvis", PD, fixed=TRUE)
  PD <- gsub("tissue, NEC", "tissue NEC", PD, fixed=TRUE)
  PD <- gsub("abortion, antepartum", "abortion antepartum", PD, fixed=TRUE)
  PD <- gsub("elsewhere, complicating", "elsewhere complicating", PD, fixed=TRUE)
  PD <- gsub("brain, supratentorial", "brain supratentorial", PD, fixed=TRUE)
  PD <- gsub("Spondylolisthesis, lumbosacral", "Spondylolisthesis lumbosacral", PD, fixed=TRUE)
  PD <- gsub("response, confused", "response confused", PD, fixed=TRUE)
  PD <- gsub("degeneration, high", "degeneration high", PD, fixed=TRUE)
  PD <- gsub("spine, cervicothoracic", "spine cervicothoracic", PD, fixed=TRUE)
  PD <- gsub("elsewhere, postpartum", "elsewhere postpartum", PD, fixed=TRUE)
  PD <- gsub("myelopathy, thoracic", "myelopathy thoracic", PD, fixed=TRUE)
  PD <- gsub("fistula, female", "fistula female", PD, fixed=TRUE)
  PD <- gsub("delivery, twins", "delivery twins", PD, fixed=TRUE)
  PD <- gsub("loss, antepartum", "loss antepartum", PD, fixed=TRUE)
  PD <- gsub("ethanol, undetermined", "ethanol undetermined", PD, fixed=TRUE)
  PD <- gsub("leg, level", "leg level", PD, fixed=TRUE)
  PD <- gsub("unspecifiedT79.6XXA", "unspecified, T79.6XXA", PD, fixed=TRUE)
  PD <- gsub("lordosis, lumbosacral", "lordosis lumbosacral", PD, fixed=TRUE)
  PD <- gsub("pre-eclampsia, complicating", "pre-eclampsia complicating", PD, fixed=TRUE)
  PD <- gsub("mother, delivered", "mother delivered", PD, fixed=TRUE)
  PD <- gsub("hemorrhage, postpartum", "hemorrhage postpartum", PD, fixed=TRUE)
  PD <- gsub("gravidarum, antepartum", "gravidarum antepartum", PD, fixed=TRUE)
  PD <- gsub("care, third", "care third", PD, fixed=TRUE)
  PD <- gsub("leukemia, BCR/ABL-positive", "leukemia BCR/ABL-positive", PD, fixed=TRUE)
  PD <- gsub("degeneration, lumbosacral", "degeneration lumbosacral", PD, fixed=TRUE)
  PD <- gsub("puerperium, delivered", "puerperium delivered", PD, fixed=TRUE)
  PD <- gsub("compression, complicating", "compression complicating", PD, fixed=TRUE)
  PD <- gsub("endometritis, postpartum", "endometritis postpartum", PD, fixed=TRUE)
  PD <- gsub("cord, complicating", "cord complicating", PD, fixed=TRUE)
  PD <- gsub("multigravida, third", "multigravida third", PD, fixed=TRUE)
  PD <- gsub("puerperium, antepartum", "puerperium antepartum", PD, fixed=TRUE)
  PD <- gsub("behavior 305.1", "behavior, 305.1", PD, fixed=TRUE)
  PD <- gsub("Postoperative shock (HCC)", "Postoperative shock", PD, fixed=TRUE)
  #### Change all sepsis definitions to be the same and retain the initial differentiating attribute ####
  PD <- gsub("0A41.9 Sepsis", "A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("995.91 Sepsis(995.91)", "A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("785.52 Septic shock(785.52)", "A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("B37.7 Candidal sepsis", "B37.7 Candidal sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.50 Gram-negative sepsis", "A41.50 Gram-negative sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.59 Other gram-negative sepsis", "A41.59 Other gram-negative sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.89 Other specified sepsis", "A41.89 Other specified sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A40.8 Other streptococcal sepsis", "A40.8 Other streptococcal sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("995.92 Severe sepsis", "995.92 Severe sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("R65.20 Severe sepsis without septic shock", "R65.20 Severe sepsis without septic shock, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A40.9 Streptococcal sepsis", "A40.9 Streptococcal sepsis, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.81 Sepsis due to Enterococcus", "A41.81 Sepsis due to Enterococcus, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.51 Sepsis due to Escherichia coli (e. coli)", "A41.51 Sepsis due to Escherichia coli (e. coli), A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.52 Sepsis due to Pseudomonas", "A41.52 Sepsis due to Pseudomonas, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.53 Sepsis due to Serratia", "A41.53 Sepsis due to Serratia, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A40.1 Sepsis due to Streptococcus B", "A40.1 Sepsis due to Streptococcus B, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A40.3 Sepsis due to Streptococcus pneumoniae", "A40.3 Sepsis due to Streptococcus pneumoniae, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.4 Sepsis due to anaerobes", "A41.4 Sepsis due to anaerobes, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.3 Sepsis due to hemophilus influenzae", "A41.3 Sepsis due to hemophilus influenzae, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.02 Sepsis due to methicillin resistant Staphylococcus aureus", "A41.02 Sepsis due to methicillin resistant Staphylococcus aureus, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.01 Sepsis due to methicillin susceptible Staphylococcus aureus", "A41.01 Sepsis due to methicillin susceptible Staphylococcus aureus, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.1 Sepsis due to other specified Staphylococcus", "A41.1 Sepsis due to other specified Staphylococcus, A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("A41.2 Sepsis due to unspecified Staphylococcus", "A41.2 Sepsis due to unspecified Staphylococcus, A41.9 Sepsis", PD, fixed=TRUE)
  #############################################################################################################################################
  #### adjust Body mass index ####
  PD <- gsub("Body mass index (BMI)", "Body mass index", PD, fixed=TRUE)
  PD <- gsub("Body mass index (bmi)", "Body mass index", PD, fixed=TRUE)
  PD <- gsub("(HCC)", "", PD, fixed=TRUE)
  PD <- gsub("Z68.25 Body mass index 25.0-25.9", "V85.21 Body mass index 25.0-25.9", PD, fixed=TRUE)
  PD <- gsub("Z68.26 Body mass index 26.0-26.9", "V85.22 Body mass index 26.0-26.9", PD, fixed=TRUE)
  PD <- gsub("Z68.27 Body mass index 27.0-27.9", "V85.23 Body mass index 27.0-27.9", PD, fixed=TRUE)
  PD <- gsub("Z68.28 Body mass index 28.0-28.9", "V85.24 Body mass index 28.0-28.9", PD, fixed=TRUE)
  PD <- gsub("Z68.29 Body mass index 29.0-29.9", "V85.25 Body mass index 29.0-29.9", PD, fixed=TRUE)
  PD <- gsub("Z68.30 Body mass index 30.0-30.9", "V85.30 Body mass index 30.0-30.9", PD, fixed=TRUE)
  PD <- gsub("Z68.31 Body mass index 31.0-31.9", "V85.31 Body mass index 31.0-31.9", PD, fixed=TRUE)
  PD <- gsub("Z68.32 Body mass index 32.0-32.9", "V85.32 Body mass index 32.0-32.9", PD, fixed=TRUE)
  PD <- gsub("Z68.33 Body mass index 33.0-33.9", "V85.33 Body mass index 33.0-33.9", PD, fixed=TRUE)
  PD <- gsub("Z68.34 Body mass index 34.0-34.9", "V85.34 Body mass index 34.0-34.9", PD, fixed=TRUE)
  PD <- gsub("V85.35 Body mass index 35.0-35.9", "Z68.35 Body mass index 35.0-35.9", PD, fixed=TRUE)
  PD <- gsub("Z68.36 Body mass index 36.0-36.9", "V85.36 Body mass index 36.0-36.9", PD, fixed=TRUE)
  PD <- gsub("Z68.37 Body mass index 37.0-37.9", "V85.37 Body mass index 37.0-37.9", PD, fixed=TRUE)
  PD <- gsub("Z68.38 Body mass index 38.0-38.9", "V85.38 Body mass index 38.0-38.9", PD, fixed=TRUE)
  PD <- gsub("Z68.39 Body mass index 39.0-39.9", "V85.39 Body mass index 39.0-39.9", PD, fixed=TRUE)
  PD <- gsub("Z68.41 Body mass index 40.0-44.9", "V85.41 Body mass index 40.0-44.9", PD, fixed=TRUE)
  PD <- gsub("Z68.42 Body mass index 45.0-49.9", "V85.42 Body mass index 45.0-49.9", PD, fixed=TRUE)
  PD <- gsub("Z68.43 Body mass index 50-59.9", "V85.43 Body mass index 50.0-59.9", PD, fixed=TRUE)
  PD <- gsub("Z68.44 Body mass index 60.0-69.9", "V85.44 Body mass index 60.0-69.9", PD, fixed=TRUE)
  PD <- gsub("V85.45 Body mass index 70 and over", "Z68.45 Body mass index 70 or greater", PD, fixed=TRUE)
  ###################################################################
  #### Remove pressure ulcer diagnoses for testing purposes only ####
  ###################################################################
  PD <- gsub("707.07 Pressure ulcer", "", PD, fixed=TRUE)
  PD <- gsub("707.01 Pressure ulcer", "", PD, fixed=TRUE)
  PD <- gsub("707.04 Pressure ulcer", "", PD, fixed=TRUE)
  PD <- gsub("707.21 Pressure ulcer I(707.21)", "", PD, fixed=TRUE)
  PD <- gsub("707.22 Pressure ulcer II(707.22)", "", PD, fixed=TRUE)
  PD <- gsub("707.23 Pressure ulcer III(707.23)", "", PD, fixed=TRUE)
  PD <- gsub("707.24 Pressure ulcer IV(707.24)", "", PD, fixed=TRUE)
  PD <- gsub("707.02 Pressure ulcer back(707.02)", "", PD, fixed=TRUE)
  PD <- gsub("707.03 Pressure ulcer back(707.03)", "", PD, fixed=TRUE)
  PD <- gsub("L89.812 Pressure ulcer of head", "", PD, fixed=TRUE)
  PD <- gsub("L89.810 Pressure ulcer of head", "", PD, fixed=TRUE)
  PD <- gsub("L89.811 Pressure ulcer of head 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.819 Pressure ulcer of head stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.322 Pressure ulcer of left buttock", "", PD, fixed=TRUE)
  PD <- gsub("L89.320 Pressure ulcer of left buttock", "", PD, fixed=TRUE)
  PD <- gsub("L89.321 Pressure ulcer of left buttock 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.323 Pressure ulcer of left buttock 3", "", PD, fixed=TRUE)
  PD <- gsub("L89.324 Pressure ulcer of left buttock 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.329 Pressure ulcer of left buttock stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.022 Pressure ulcer of left elbow", "", PD, fixed=TRUE)
  PD <- gsub("L89.622 Pressure ulcer of left heel", "", PD, fixed=TRUE)
  PD <- gsub("L89.620 Pressure ulcer of left heel", "", PD, fixed=TRUE)
  PD <- gsub("L89.621 Pressure ulcer of left heel 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.629 Pressure ulcer of left heel stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.220 Pressure ulcer of left hip", "", PD, fixed=TRUE)
  PD <- gsub("L89.222 Pressure ulcer of left hip", "", PD, fixed=TRUE)
  PD <- gsub("L89.223 Pressure ulcer of left hip 3", "", PD, fixed=TRUE)
  PD <- gsub("L89.224 Pressure ulcer of left hip 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.229 Pressure ulcer of left hip stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.142 Pressure ulcer of left lower back", "", PD, fixed=TRUE)
  PD <- gsub("L89.892 Pressure ulcer of other site", "", PD, fixed=TRUE)
  PD <- gsub("L89.890 Pressure ulcer of other site", "", PD, fixed=TRUE)
  PD <- gsub("L89.891 Pressure ulcer of other site 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.894 Pressure ulcer of other site 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.899 Pressure ulcer of other site stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.510 Pressure ulcer of right ankle", "", PD, fixed=TRUE)
  PD <- gsub("L89.312 Pressure ulcer of right buttock", "", PD, fixed=TRUE)
  PD <- gsub("L89.310 Pressure ulcer of right buttock", "", PD, fixed=TRUE)
  PD <- gsub("L89.311 Pressure ulcer of right buttock 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.313 Pressure ulcer of right buttock 3", "", PD, fixed=TRUE)
  PD <- gsub("L89.314 Pressure ulcer of right buttock 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.319 Pressure ulcer of right buttock stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.612 Pressure ulcer of right heel", "", PD, fixed=TRUE)
  PD <- gsub("L89.610 Pressure ulcer of right heel", "", PD, fixed=TRUE)
  PD <- gsub("L89.613 Pressure ulcer of right heel 3", "", PD, fixed=TRUE)
  PD <- gsub("L89.619 Pressure ulcer of right heel stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.212 Pressure ulcer of right hip", "", PD, fixed=TRUE)
  PD <- gsub("L89.210 Pressure ulcer of right hip", "", PD, fixed=TRUE)
  PD <- gsub("L89.214 Pressure ulcer of right hip 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.112 Pressure ulcer of right upper back", "", PD, fixed=TRUE)
  PD <- gsub("L89.111 Pressure ulcer of right upper back 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.152 Pressure ulcer of sacral region", "", PD, fixed=TRUE)
  PD <- gsub("L89.150 Pressure ulcer of sacral region", "", PD, fixed=TRUE)
  PD <- gsub("L89.151 Pressure ulcer of sacral region 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.153 Pressure ulcer of sacral region 3", "", PD, fixed=TRUE)
  PD <- gsub("L89.154 Pressure ulcer of sacral region 4", "", PD, fixed=TRUE)
  PD <- gsub("L89.159 Pressure ulcer of sacral region stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.509 Pressure ulcer of unspecified ankle stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.302 Pressure ulcer of unspecified buttock", "", PD, fixed=TRUE)
  PD <- gsub("L89.301 Pressure ulcer of unspecified buttock 1", "", PD, fixed=TRUE)
  PD <- gsub("L89.309 Pressure ulcer of unspecified buttock stage", "", PD, fixed=TRUE)
  PD <- gsub("L89.202 Pressure ulcer of unspecified hip", "", PD, fixed=TRUE)
  PD <- gsub("L89.102 Pressure ulcer of unspecified part of back", "", PD, fixed=TRUE)
  PD <- gsub("L89.100 Pressure ulcer of unspecified part of back", "", PD, fixed=TRUE)
  PD <- gsub("L89.92 Pressure ulcer of unspecified site", "", PD, fixed=TRUE)
  PD <- gsub("L89.90 Pressure ulcer of unspecified site stage", "", PD, fixed=TRUE)
  PD <- gsub("707.00 Pressure ulcer site(707.00)", "", PD, fixed=TRUE)
  PD <- gsub("707.09 Pressure ulcer site(707.09)", "", PD, fixed=TRUE)
  PD <- gsub("707.20 Pressure ulcer stage(707.20)", "", PD, fixed=TRUE)
  PD <- gsub("707.06 Pressure ulcer(707.06)", "", PD, fixed=TRUE)
  PD <- gsub("707.25 Pressure ulcer(707.25)", "", PD, fixed=TRUE)
  PD <- gsub("707.05 Pressure ulcertock(707.05)", "", PD, fixed=TRUE)
  #### merge duplicated codes into one code ####
  ##############################################
  PD <- gsub("V49.87 Physical restraints status", "Z78.1 Physical restraint status", PD, fixed=TRUE)
  PD <- gsub("38.9 Unspecified septicemia(038.9)", "A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("Severe sepsis(995.92)", "Severe sepsis", PD, fixed=TRUE)
  PD <- gsub("R65.21 Severe sepsis with septic shock", "995.92 Severe sepsis", PD, fixed=TRUE)
  PD <- gsub("N17.9 Acute kidney failure", "584.9 Acute kidney failure", PD, fixed=TRUE)
  PD <- gsub("I10 Essential (primary) hypertension", "401.9 Unspecified essential hypertension", PD, fixed=TRUE)
  PD <- gsub("I71.3 Abdominal aortic aneurysm", "I71.4 Abdominal aortic aneurysm", PD, fixed=TRUE)
  PD <- gsub("789.07 Abdominal pain", "789.06 Abdominal pain", PD, fixed=TRUE)
  PD <- gsub("790.92 Abnormal coagulation profile", "R79.1 Abnormal coagulation profile", PD, fixed=TRUE)
  PD <- gsub("S90.811A Abrasion foot", "S90.812A Abrasion foot", PD, fixed=TRUE)
  PD <- gsub("S80.812A Abrasion lower leg", "S80.811A Abrasion lower leg", PD, fixed=TRUE)
  PD <- gsub("276.2 Acidosis", "E87.2 Acidosis", PD, fixed=TRUE)
  PD <- gsub("V88.01 Acquired absence of both cervix and uterus", "Z90.710 Acquired absence of both cervix and uterus", PD, fixed=TRUE)
  PD <- gsub("V45.73 Acquired absence of kidney", "Z90.5 Acquired absence of kidney", PD, fixed=TRUE)
  PD <- gsub("466.0 Acute bronchitis", "J20.9 Acute bronchitis", PD, fixed=TRUE)
  PD <- gsub("575.0 Acute cholecystitis", "K81.0 Acute cholecystitis", PD, fixed=TRUE)
  PD <- gsub("338.11 Acute pain due to trauma", "G89.11 Acute pain due to trauma", PD, fixed=TRUE)
  PD <- gsub("577.0 Acute pancreatitis", "K85.9 Acute pancreatitis", PD, fixed=TRUE)
  PD <- gsub("420.90 Acute pericarditis", "I30.9 Acute pericarditis", PD, fixed=TRUE)
  PD <- gsub("285.1 Acute posthemorrhagic anemia", "D62 Acute posthemorrhagic anemia", PD, fixed=TRUE)
  PD <- gsub("461.9 Acute sinusitis", "J01.90 Acute sinusitis", PD, fixed=TRUE)
  PD <- gsub("309.24 Adjustment disorder with anxiety", "F43.22 Adjustment disorder with anxiety", PD, fixed=TRUE)
  PD <- gsub("309.0 Adjustment disorder with depressed mood", "F43.21 Adjustment disorder with depressed mood", PD, fixed=TRUE)
  PD <- gsub("309.28 Adjustment disorder with mixed anxiety and depressed mood", "F43.23 Adjustment disorder with mixed anxiety and depressed mood", PD, fixed=TRUE)
  PD <- gsub("783.7 Adult failure to thrive", "R62.7 Adult failure to thrive", PD, fixed=TRUE)
  PD <- gsub("T50.995A Adverse effect of other drugs and biological substances", "T50.995S Adverse effect of other drugs and biological substances", PD, fixed=TRUE)
  PD <- gsub("H25.12 Age-related nuclear cataract eye", "H25.10 Age-related nuclear cataract eye", PD, fixed=TRUE)
  PD <- gsub("K05.21 Aggressive periodontitis", "523.31 Aggressive periodontitis", PD, fixed=TRUE)
  PD <- gsub("305.00 Alcohol abuse", "F10.10 Alcohol abuse", PD, fixed=TRUE)
  PD <- gsub("276.3 Alkalosis", "E87.3 Alkalosis", PD, fixed=TRUE)
  PD <- gsub("Z91.018 Allergy to other foods", "V15.05 Allergy to other foods", PD, fixed=TRUE)
  PD <- gsub("V15.04 Allergy to seafood", "Z91.013 Allergy to seafood", PD, fixed=TRUE)
  PD <- gsub("780.97 Altered mental status", "R41.82 Altered mental status", PD, fixed=TRUE)
  PD <- gsub("331.0 Alzheimer's disease", "G30.9 Alzheimer's disease", PD, fixed=TRUE)
  PD <- gsub("565.0 Anal fissure", "K60.2 Anal fissure", PD, fixed=TRUE)
  PD <- gsub("285.9 Anemia", "D64.9 Anemia", PD, fixed=TRUE)
  PD <- gsub("285.22 Anemia in neoplastic disease", "D63.0 Anemia in neoplastic disease", PD, fixed=TRUE)
  PD <- gsub("379.41 Anisocoria", "H57.02 Anisocoria", PD, fixed=TRUE)
  PD <- gsub("R63.0 Anorexia", "783.0 Anorexia", PD, fixed=TRUE)
  PD <- gsub("784.3 Aphasia", "R47.01 Aphasia", PD, fixed=TRUE)
  PD <- gsub("447.6 Arteritis", "I77.6 Arteritis", PD, fixed=TRUE)
  PD <- gsub("V45.4 Arthrodesis status", "Z98.1 Arthrodesis status", PD, fixed=TRUE)
  PD <- gsub("I70.262 Atherosclerosis of native arteries of extremities with gangrene leg", "I70.261 Atherosclerosis of native arteries of extremities with gangrene leg", PD, fixed=TRUE)
  PD <- gsub("426.10 Atrioventricular block", "I44.2 Atrioventricular block", PD, fixed=TRUE)
  PD <- gsub("I44.1 Atrioventricular block degree", "I44.0 Atrioventricular block degree", PD, fixed=TRUE)
  PD <- gsub("V49.83 Awaiting organ transplant status", "Z76.82 Awaiting organ transplant status", PD, fixed=TRUE)
  PD <- gsub("790.7 Bacteremia", "R78.81 Bacteremia", PD, fixed=TRUE)
  PD <- gsub("V45.86 Bariatric surgery status", "Z98.84 Bariatric surgery status", PD, fixed=TRUE)
  PD <- gsub("V49.84 Bed confinement status", "Z74.01 Bed confinement status", PD, fixed=TRUE)
  PD <- gsub("G51.0 Bell's palsy", "351.0 Bell's palsy", PD, fixed=TRUE)
  PD <- gsub("348.2 Benign intracranial hypertension", "G93.2 Benign intracranial hypertension", PD, fixed=TRUE)
  PD <- gsub("296.80 Bipolar disorder", "F31.9 Bipolar disorder", PD, fixed=TRUE)
  PD <- gsub("H54.12 Blindness eye vision right eye", "H54.42 Blindness eye vision right eye", PD, fixed=TRUE)
  PD <- gsub("T80.211A Bloodstream infection due to central venous catheter", "999.32 Bloodstream infection due to central venous catheter", PD, fixed=TRUE)
  PD <- gsub("490 Bronchitis specified as acute or chronic", "J40 Bronchitis specified as acute or chronic", PD, fixed=TRUE)
  PD <- gsub("592.0 Calculus of kidney", "N20.0 Calculus of kidney", PD, fixed=TRUE)
  PD <- gsub("B37.3 Candidiasis of vulva and vagina", "112.1 Candidiasis of vulva and vagina", PD, fixed=TRUE)
  PD <- gsub("305.21 Cannabis abuse", "F12.10 Cannabis abuse", PD, fixed=TRUE)
  PD <- gsub("305.20 Cannabis abuse", "F12.10 Cannabis abuse", PD, fixed=TRUE)
  PD <- gsub("423.3 Cardiac tamponade", "I31.4 Cardiac tamponade", PD, fixed=TRUE)
  PD <- gsub("V02.54 Carrier or suspected carrier of methicillin resistant Staphylococcus aureus", "Z22.322 Carrier or suspected carrier of methicillin resistant Staphylococcus aureus", PD, fixed=TRUE)
  PD <- gsub("Z98.42 Cataract extraction status eye", "Z98.49 Cataract extraction status eye", PD, fixed=TRUE)
  PD <- gsub("Z98.41 Cataract extraction status eye", "Z98.49 Cataract extraction status eye", PD, fixed=TRUE)
  PD <- gsub("437.3 Cerebral aneurysm", "I67.1 Cerebral aneurysm", PD, fixed=TRUE)
  PD <- gsub("I67.2 Cerebral atherosclerosis", "437.0 Cerebral atherosclerosis", PD, fixed=TRUE)
  PD <- gsub("723.1 Cervicalgia", "M54.2 Cervicalgia", PD, fixed=TRUE)
  PD <- gsub("786.50 Chest pain", "R07.9 Chest pain", PD, fixed=TRUE)
  PD <- gsub("576.1 Cholangitis", "K83.0 Cholangitis", PD, fixed=TRUE)
  PD <- gsub("575.10 Cholecystitis", "K81.9 Cholecystitis", PD, fixed=TRUE)
  PD <- gsub("585.9 Chronic kidney disease", "N18.3 Chronic kidney disease", PD, fixed=TRUE)
  PD <- gsub("N18.9 Chronic kidney disease", "N18.3 Chronic kidney disease", PD, fixed=TRUE)
  PD <- gsub("J32.0 Chronic maxillary sinusitis", "473.0 Chronic maxillary sinusitis", PD, fixed=TRUE)
  PD <- gsub("M86.452 Chronic osteomyelitis with draining sinus femur", "M86.451 Chronic osteomyelitis with draining sinus femur", PD, fixed=TRUE)
  PD <- gsub("338.4 Chronic pain syndrome", "G89.4 Chronic pain syndrome", PD, fixed=TRUE)
  PD <- gsub("573.0 Chronic passive congestion of liver", "K76.1 Chronic passive congestion of liver", PD, fixed=TRUE)
  PD <- gsub("523.42 Chronic periodontitis", "523.40 Chronic periodontitis", PD, fixed=TRUE)
  PD <- gsub("839.01 Closed dislocation cervical vertebra", "839.06 Closed dislocation cervical vertebra", PD, fixed=TRUE)
  PD <- gsub("839.07 Closed dislocation cervical vertebra", "839.06 Closed dislocation cervical vertebra", PD, fixed=TRUE)
  PD <- gsub("305.60 Cocaine abuse", "F14.10 Cocaine abuse", PD, fixed=TRUE)
  PD <- gsub("R40.2322 Coma scale motor response arrival to emergency department", "R40.2312 Coma scale motor response arrival to emergency department", PD, fixed=TRUE)
  PD <- gsub("R40.2143 Coma scale open hospital admission", "R40.2113 Coma scale open hospital admission", PD, fixed=TRUE)
  PD <- gsub("459.2 Compression of vein", "I87.1 Compression of vein", PD, fixed=TRUE)
  PD <- gsub("850.11 Concussion with loss of consciousness of 30 minutes or less", "S06.0X1A Concussion with loss of consciousness of 30 minutes or less", PD, fixed=TRUE)
  PD <- gsub("746.4 Congenital insufficiency of aortic valve", "Q23.1 Congenital insufficiency of aortic valve", PD, fixed=TRUE)
  PD <- gsub("S30.1XXA Contusion of abdominal wall", "922.2 Contusion of abdominal wall", PD, fixed=TRUE)
  PD <- gsub("S27.321A Contusion of lung", "S27.322A Contusion of lung", PD, fixed=TRUE)
  PD <- gsub("S27.329A Contusion of lung", "S27.322A Contusion of lung", PD, fixed=TRUE)
  PD <- gsub("R05 Cough", "786.2 Cough", PD, fixed=TRUE)
  PD <- gsub("359.81 Critical illness myopathy", "G72.81 Critical illness myopathy", PD, fixed=TRUE)
  PD <- gsub("276.51 Dehydration", "E86.0 Dehydration", PD, fixed=TRUE)
  PD <- gsub("294.21 Dementia behavioral disturbance", "294.20 Dementia behavioral disturbance", PD, fixed=TRUE)
  PD <- gsub("V46.2 Dependence on supplemental oxygen", "Z99.81 Dependence on supplemental oxygen", PD, fixed=TRUE)
  PD <- gsub("787.91 Diarrhea", "R19.7 Diarrhea", PD, fixed=TRUE)
  PD <- gsub("519.4 Disorders of diaphragm", "J98.6 Disorders of diaphragm", PD, fixed=TRUE)
  PD <- gsub("S92.322A Displaced fracture of second metatarsal bone foot for closed fracture", "S92.321A Displaced fracture of second metatarsal bone foot for closed fracture", PD, fixed=TRUE)
  PD <- gsub("998.32 Disruption of external operation (surgical) wound", "T81.31XA Disruption of external operation (surgical) wound", PD, fixed=TRUE)
  PD <- gsub("998.31 Disruption of internal operation (surgical) wound", "T81.31XA Disruption of internal operation (surgical) wound", PD, fixed=TRUE)
  PD <- gsub("T81.32XA Disruption of internal operation (surgical) wound", "T81.31XA Disruption of internal operation (surgical) wound", PD, fixed=TRUE)
  PD <- gsub("998.32 Disruption of internal operation (surgical) wound", "T81.31XA Disruption of internal operation (surgical) wound", PD, fixed=TRUE)
  PD <- gsub("787.23 Dysphagia phase", "R13.13 Dysphagia phase", PD, fixed=TRUE)
  PD <- gsub("787.24 Dysphagia phase", "R13.13 Dysphagia phase", PD, fixed=TRUE)
  PD <- gsub("R13.14 Dysphagia phase", "R13.13 Dysphagia phase", PD, fixed=TRUE)
  PD <- gsub("R13.11 Dysphagiaal phase", "787.21 Dysphagiaal phase", PD, fixed=TRUE)
  PD <- gsub("787.22 Dysphagiaopharyngeal phase", "R13.12 Dysphagiaopharyngeal phase", PD, fixed=TRUE)
  PD <- gsub("784.42 Dysphonia", "R49.0 Dysphonia", PD, fixed=TRUE)
  PD <- gsub("782.3 Edema", "R60.9 Edema", PD, fixed=TRUE)
  PD <- gsub("478.6 Edema of larynx", "J38.4 Edema of larynx", PD, fixed=TRUE)
  PD <- gsub("M25.462 Effusion knee", "M25.461 Effusion knee", PD, fixed=TRUE)
  PD <- gsub("348.30 Encephalopathy", "G93.40 Encephalopathy", PD, fixed=TRUE)
  PD <- gsub("V66.7 Encounter for palliative care", "Z51.5 Encounter for palliative care", PD, fixed=TRUE)
  PD <- gsub("G40.901 Epilepsy intractable status epilepticus", "G40.909 Epilepsy intractable status epilepticus", PD, fixed=TRUE)
  PD <- gsub("G40.911 Epilepsy status epilepticus", "G40.919 Epilepsy status epilepticus", PD, fixed=TRUE)
  PD <- gsub("784.7 Epistaxis", "R04.0 Epistaxis", PD, fixed=TRUE)
  PD <- gsub("530.10 Esophagitis", "K20.9 Esophagitis", PD, fixed=TRUE)
  PD <- gsub("401.1 Essential hypertension", "401.0 Essential hypertension", PD, fixed=TRUE)
  PD <- gsub("781.94 Facial weakness", "R29.810 Facial weakness", PD, fixed=TRUE)
  PD <- gsub("780.60 Fever", "R50.9 Fever", PD, fixed=TRUE)
  PD <- gsub("780.61 Fever presenting with conditions classified elsewhere", "R50.81 Fever presenting with conditions classified elsewhere", PD, fixed=TRUE)
  PD <- gsub("R15.9 Full incontinence of feces", "787.60 Full incontinence of feces", PD, fixed=TRUE)
  PD <- gsub("536.3 Gastroparesis", "K31.84 Gastroparesis", PD, fixed=TRUE)
  PD <- gsub("274.9 Gout", "M10.9 Gout", PD, fixed=TRUE)
  PD <- gsub("599.71 Gross hematuria", "R31.0 Gross hematuria", PD, fixed=TRUE)
  PD <- gsub("428.9 Heart failure", "I50.9 Heart failure", PD, fixed=TRUE)
  PD <- gsub("578.0 Hematemesis", "K92.0 Hematemesis", PD, fixed=TRUE)
  PD <- gsub("599.70 Hematuria", "R31.9 Hematuria", PD, fixed=TRUE)
  PD <- gsub("786.30 Hemoptysis", "R04.2 Hemoptysis", PD, fixed=TRUE)
  PD <- gsub("459.0 Hemorrhage", "R58 Hemorrhage", PD, fixed=TRUE)
  PD <- gsub("789.1 Hepatomegaly", "R16.0 Hepatomegaly", PD, fixed=TRUE)
  PD <- gsub("B20 Human immunodeficiency virus (HIV) disease", "042 Human immunodeficiency virus (HIV) disease", PD, fixed=TRUE)
  PD <- gsub("275.42 Hypercalcemia", "E83.52 Hypercalcemia", PD, fixed=TRUE)
  PD <- gsub("E21.3 Hyperparathyroidism", "252.00 Hyperparathyroidism", PD, fixed=TRUE)
  PD <- gsub("H35.033 Hypertensive retinopathy", "362.11 Hypertensive retinopathy", PD, fixed=TRUE)
  PD <- gsub("275.41 Hypocalcemia", "E83.51 Hypocalcemia", PD, fixed=TRUE)
  PD <- gsub("251.2 Hypoglycemia", "E16.2 Hypoglycemia", PD, fixed=TRUE)
  PD <- gsub("458.9 Hypotension", "I95.9 Hypotension", PD, fixed=TRUE)
  PD <- gsub("276.52 Hypovolemia", "E86.1 Hypovolemia", PD, fixed=TRUE)
  PD <- gsub("799.02 Hypoxemia", "R09.02 Hypoxemia", PD, fixed=TRUE)
  PD <- gsub("T83.51XA Infection and inflammatory reaction due to indwelling urinary catheter", "996.64 Infection and inflammatory reaction due to indwelling urinary catheter", PD, fixed=TRUE)
  PD <- gsub("T87.43 Infection of amputation stump lower extremity", "T87.44 Infection of amputation stump lower extremity", PD, fixed=TRUE)
  PD <- gsub("780.52 Insomnia", "G47.00 Insomnia", PD, fixed=TRUE)
  PD <- gsub("280.9 Iron deficiency anemia", "D50.9 Iron deficiency anemia", PD, fixed=TRUE)
  PD <- gsub("280.0 Iron deficiency anemia secondary to blood loss (chronic)", "D50.0 Iron deficiency anemia secondary to blood loss (chronic)", PD, fixed=TRUE)
  PD <- gsub("S81.812A Laceration without foreign body lower leg", "S81.811A Laceration without foreign body lower leg", PD, fixed=TRUE)
  PD <- gsub("V64.41 Laparoscopic surgical procedure converted to open procedure", "Z53.31 Laparoscopic surgical procedure converted to open procedure", PD, fixed=TRUE)
  PD <- gsub("H54.8 Legal blindness defined in USA", "369.4 Legal blindness defined in USA", PD, fixed=TRUE)
  PD <- gsub("218.9 Leiomyoma of uterus", "D25.9 Leiomyoma of uterus", PD, fixed=TRUE)
  PD <- gsub("D72.823 Leukemoid reaction", "288.62 Leukemoid reaction", PD, fixed=TRUE)
  PD <- gsub("864.03 Liver laceration mention of open wound into cavity", "864.02 Liver laceration mention of open wound into cavity", PD, fixed=TRUE)
  PD <- gsub("G40.209 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with complex partial seizures intractable status epilepticus", "G40.201 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with complex partial seizures intractable status epilepticus", PD, fixed=TRUE)
  PD <- gsub("G40.111 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with simple partial seizures status epilepticus", "G40.119 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with simple partial seizures status epilepticus", PD, fixed=TRUE)
  PD <- gsub("715.34 Localized osteoarthrosis not specified whether primary or secondary", "715.33 Localized osteoarthrosis not specified whether primary or secondary", PD, fixed=TRUE)
  PD <- gsub("426.82 Long QT syndrome", "I45.81 Long QT syndrome", PD, fixed=TRUE)
  PD <- gsub("V58.61 Long term (current) use of anticoagulants", "Z79.01 Long term (current) use of anticoagulants", PD, fixed=TRUE)
  PD <- gsub("V49.76 Lower limb amputation knee", "V49.75 Lower limb amputation knee", PD, fixed=TRUE)
  PD <- gsub("F33.1 Major depressive disorder", "F32.9 Major depressive disorder", PD, fixed=TRUE)
  PD <- gsub("F32.1 Major depressive disorder", "F32.9 Major depressive disorder", PD, fixed=TRUE)
  PD <- gsub("F33.9 Major depressive disorder", "F32.9 Major depressive disorder", PD, fixed=TRUE)
  PD <- gsub("F33.0 Major depressive disorder", "F32.9 Major depressive disorder", PD, fixed=TRUE)
  PD <- gsub("296.20 Major depressive disorder", "F32.9 Major depressive disorder", PD, fixed=TRUE)
  PD <- gsub("F32.2 Major depressive disorder severe without psychotic features", "F33.2 Major depressive disorder severe without psychotic features", PD, fixed=TRUE)
  PD <- gsub("802.5 Malar and maxillary bones fracture", "802.4 Malar and maxillary bones fracture", PD, fixed=TRUE)
  PD <- gsub("R18.0 Malignant ascites", "789.51 Malignant ascites", PD, fixed=TRUE)
  PD <- gsub("C34.12 Malignant neoplasm of upper lobe bronchus or lung", "C34.11 Malignant neoplasm of upper lobe bronchus or lung", PD, fixed=TRUE)
  PD <- gsub("511.81 Malignant pleural effusion", "J91.0 Malignant pleural effusion", PD, fixed=TRUE)
  PD <- gsub("S02.40CA Maxillary fracture side for closed fracture", "S02.401A Maxillary fracture side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("322.9 Meningitis", "G03.9 Meningitis", PD, fixed=TRUE)
  PD <- gsub("348.31 Metabolic encephalopathy", "G93.41 Metabolic encephalopathy", PD, fixed=TRUE)
  PD <- gsub("G43.901 Migraine intractable status migrainosus", "G43.909 Migraine intractable status migrainosus", PD, fixed=TRUE)
  PD <- gsub("317 Mild intellectual disabilities", "F70 Mild intellectual disabilities", PD, fixed=TRUE)
  PD <- gsub("272.2 Mixed hyperlipidemia", "E78.2 Mixed hyperlipidemia", PD, fixed=TRUE)
  PD <- gsub("F71 Moderate intellectual disabilities", "318.0 Moderate intellectual disabilities", PD, fixed=TRUE)
  PD <- gsub("S22.42XA Multiple fractures of ribs side for closed fracture", "S22.41XA Multiple fractures of ribs side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("728.87 Muscle weakness (generalized)", "M62.81 Muscle weakness (generalized)", PD, fixed=TRUE)
  PD <- gsub("238.75 Myelodysplastic syndrome", "D46.9 Myelodysplastic syndrome", PD, fixed=TRUE)
  PD <- gsub("G25.3 Myoclonus", "333.2 Myoclonus", PD, fixed=TRUE)
  PD <- gsub("359.9 Myopathy", "G72.9 Myopathy", PD, fixed=TRUE)
  PD <- gsub("802.1 Nasal bones fracture", "802.0 Nasal bones fracture", PD, fixed=TRUE)
  PD <- gsub("787.01 Nausea with vomiting", "R11.2 Nausea with vomiting", PD, fixed=TRUE)
  PD <- gsub("T87.53 Necrosis of amputation stump lower extremity", "T87.54 Necrosis of amputation stump lower extremity", PD, fixed=TRUE)
  PD <- gsub("G89.3 Neoplasm related pain (acute) (chronic)", "338.3 Neoplasm related pain (acute) (chronic)", PD, fixed=TRUE)
  PD <- gsub("564.81 Neurogenic bowel", "K59.2 Neurogenic bowel", PD, fixed=TRUE)
  PD <- gsub("288.00 Neutropenia", "D70.9 Neutropenia", PD, fixed=TRUE)
  PD <- gsub("F17.210 Nicotine dependence", "F17.200 Nicotine dependence", PD, fixed=TRUE)
  PD <- gsub("305.71 Nondependent amphetamine or related acting sympathomimetic abuse", "305.70 Nondependent amphetamine or related acting sympathomimetic abuse", PD, fixed=TRUE)
  PD <- gsub("729.73 Nontraumatic compartment syndrome of abdomen", "M79.A3 Nontraumatic compartment syndrome of abdomen", PD, fixed=TRUE)
  PD <- gsub("729.92 Nontraumatic hematoma of soft tissue", "M79.81 Nontraumatic hematoma of soft tissue", PD, fixed=TRUE)
  PD <- gsub("I61.9 Nontraumatic intracerebral hemorrhage", "I61.5 Nontraumatic intracerebral hemorrhage", PD, fixed=TRUE)
  PD <- gsub("I61.0 Nontraumatic intracerebral hemorrhage in hemisphere", "I61.1 Nontraumatic intracerebral hemorrhage in hemisphere", PD, fixed=TRUE)
  PD <- gsub("278.00 Obesity", "E66.9 Obesity", PD, fixed=TRUE)
  PD <- gsub("576.2 Obstruction of bile duct", "K83.1 Obstruction of bile duct", PD, fixed=TRUE)
  PD <- gsub("331.4 Obstructive hydrocephalus", "G91.1 Obstructive hydrocephalus", PD, fixed=TRUE)
  PD <- gsub("327.23 Obstructive sleep apnea (adult) (pediatric)", "G47.33 Obstructive sleep apnea (adult) (pediatric)", PD, fixed=TRUE)
  PD <- gsub("412 Old myocardial infarction", "I25.2 Old myocardial infarction", PD, fixed=TRUE)
  PD <- gsub("458.0 Orthostatic hypotension", "I95.1 Orthostatic hypotension", PD, fixed=TRUE)
  PD <- gsub("R73.09 Other abnormal glucose", "790.29 Other abnormal glucose", PD, fixed=TRUE)
  PD <- gsub("M86.152 Other acute osteomyelitis femur", "M86.151 Other acute osteomyelitis femur", PD, fixed=TRUE)
  PD <- gsub("512.84 Other air leak", "J93.82 Other air leak", PD, fixed=TRUE)
  PD <- gsub("303.91 Other and unspecified alcohol dependence drinking behavior", "303.90 Other and unspecified alcohol dependence drinking behavior", PD, fixed=TRUE)
  PD <- gsub("789.59 Other ascites", "R18.8 Other ascites", PD, fixed=TRUE)
  PD <- gsub("786.59 Other chest pain", "R07.89 Other chest pain", PD, fixed=TRUE)
  PD <- gsub("338.29 Other chronic pain", "G89.29 Other chronic pain", PD, fixed=TRUE)
  PD <- gsub("T81.89XA Other complications of procedures", "T81.89XS Other complications of procedures", PD, fixed=TRUE)
  PD <- gsub("K59.09 Other constipation", "564.09 Other constipation", PD, fixed=TRUE)
  PD <- gsub("J38.3 Other diseases of vocal cords", "478.5 Other diseases of vocal cords", PD, fixed=TRUE)
  PD <- gsub("787.29 Other dysphagia", "R13.19 Other dysphagia", PD, fixed=TRUE)
  PD <- gsub("348.39 Other encephalopathy", "G93.49 Other encephalopathy", PD, fixed=TRUE)
  PD <- gsub("530.19 Other esophagitis", "K20.8 Other esophagitis", PD, fixed=TRUE)
  PD <- gsub("802.9 Other facial bones fracture", "802.8 Other facial bones fracture", PD, fixed=TRUE)
  PD <- gsub("276.69 Other fluid overload", "E87.79 Other fluid overload", PD, fixed=TRUE)
  PD <- gsub("G40.401 Other generalized epilepsy and epileptic syndromes intractable status epilepticus", "G40.409 Other generalized epilepsy and epileptic syndromes intractable status epilepticus", PD, fixed=TRUE)
  PD <- gsub("N48.29 Other inflammatory disorders of penis", "607.2 Other inflammatory disorders of penis", PD, fixed=TRUE)
  PD <- gsub("R31.29 Other microscopic hematuria", "R31.2 Other microscopic hematuria", PD, fixed=TRUE)
  PD <- gsub("793.19 Other nonspecific abnormal finding of lung field", "R91.8 Other nonspecific abnormal finding of lung field", PD, fixed=TRUE)
  PD <- gsub("512.89 Other pneumothorax", "J93.83 Other pneumothorax", PD, fixed=TRUE)
  PD <- gsub("287.49 Other secondary thrombocytopenia", "D69.59 Other secondary thrombocytopenia", PD, fixed=TRUE)
  PD <- gsub("M85.80 Other specified disorders of bone density and structure site", "M85.88 Other specified disorders of bone density and structure site", PD, fixed=TRUE)
  PD <- gsub("246.8 Other specified disorders of thyroid", "E07.89 Other specified disorders of thyroid", PD, fixed=TRUE)
  PD <- gsub("Z98.890 Other specified postprocedural states", "Z98.89 Other specified postprocedural states", PD, fixed=TRUE)
  PD <- gsub("M65.822 Other synovitis and tenosynovitis upper arm", "M65.821 Other synovitis and tenosynovitis upper arm", PD, fixed=TRUE)
  PD <- gsub("J38.02 Paralysis of vocal cords and larynx", "J38.01 Paralysis of vocal cords and larynx", PD, fixed=TRUE)
  PD <- gsub("J38.00 Paralysis of vocal cords and larynx", "J38.01 Paralysis of vocal cords and larynx", PD, fixed=TRUE)
  PD <- gsub("K22.3 Perforation of esophagus", "530.4 Perforation of esophagus", PD, fixed=TRUE)
  PD <- gsub("522.5 Periapical abscess without sinus", "K04.7 Periapical abscess without sinus", PD, fixed=TRUE)
  PD <- gsub("443.9 Peripheral vascular disease", "I73.9 Peripheral vascular disease", PD, fixed=TRUE)
  PD <- gsub("V87.41 Personal history of antineoplastic chemotherapy", "Z92.21 Personal history of antineoplastic chemotherapy", PD, fixed=TRUE)
  PD <- gsub("V12.41 Personal history of benign neoplasm of the brain", "Z86.011 Personal history of benign neoplasm of the brain", PD, fixed=TRUE)
  PD <- gsub("V10.3 Personal history of malignant neoplasm of breast", "Z85.3 Personal history of malignant neoplasm of breast", PD, fixed=TRUE)
  PD <- gsub("V10.41 Personal history of malignant neoplasm of cervix uteri", "Z85.41 Personal history of malignant neoplasm of cervix uteri", PD, fixed=TRUE)
  PD <- gsub("V10.03 Personal history of malignant neoplasm of esophagus", "Z85.01 Personal history of malignant neoplasm of esophagus", PD, fixed=TRUE)
  PD <- gsub("V10.21 Personal history of malignant neoplasm of larynx", "Z85.21 Personal history of malignant neoplasm of larynx", PD, fixed=TRUE)
  PD <- gsub("V10.07 Personal history of malignant neoplasm of liver", "Z85.05 Personal history of malignant neoplasm of liver", PD, fixed=TRUE)
  PD <- gsub("Z85.43 Personal history of malignant neoplasm of ovary", "V10.43 Personal history of malignant neoplasm of ovary", PD, fixed=TRUE)
  PD <- gsub("V10.46 Personal history of malignant neoplasm of prostate", "Z85.46 Personal history of malignant neoplasm of prostate", PD, fixed=TRUE)
  PD <- gsub("V10.87 Personal history of malignant neoplasm of thyroid", "Z85.850 Personal history of malignant neoplasm of thyroid", PD, fixed=TRUE)
  PD <- gsub("V10.83 Personal history of other malignant neoplasm of skin", "Z85.828 Personal history of other malignant neoplasm of skin", PD, fixed=TRUE)
  PD <- gsub("Z87.11 Personal history of peptic ulcer disease", "V12.71 Personal history of peptic ulcer disease", PD, fixed=TRUE)
  PD <- gsub("V12.55 Personal history of pulmonary embolism", "Z86.711 Personal history of pulmonary embolism", PD, fixed=TRUE)
  PD <- gsub("V12.53 Personal history of sudden cardiac arrest", "Z86.74 Personal history of sudden cardiac arrest", PD, fixed=TRUE)
  PD <- gsub("V15.52 Personal history of traumatic brain injury", "Z87.820 Personal history of traumatic brain injury", PD, fixed=TRUE)
  PD <- gsub("V13.01 Personal history of urinary calculi", "Z87.442 Personal history of urinary calculi", PD, fixed=TRUE)
  PD <- gsub("482.40 Pneumonia due to Staphylococcus", "J15.20 Pneumonia due to Staphylococcus", PD, fixed=TRUE)
  PD <- gsub("452 Portal vein thrombosis", "I81 Portal vein thrombosis", PD, fixed=TRUE)
  PD <- gsub("N95.0 Postmenopausal bleeding", "627.1 Postmenopausal bleeding", PD, fixed=TRUE)
  PD <- gsub("998.02 Postoperative shock (HCC)", "998.01 Postoperative shock (HCC)", PD, fixed=TRUE)
  PD <- gsub("998.09 Postoperative shock", "998.01 Postoperative shock", PD, fixed=TRUE)
  PD <- gsub("998.02 Postoperative shock", "998.01 Postoperative shock", PD, fixed=TRUE)
  PD <- gsub("V45.2 Presence of cerebrospinal fluid drainage device", "Z98.2 Presence of cerebrospinal fluid drainage device", PD, fixed=TRUE)
  PD <- gsub("M19.011 Primary osteoarthritis shoulder", "M19.012 Primary osteoarthritis shoulder", PD, fixed=TRUE)
  PD <- gsub("318.2 Profound intellectual disabilities", "F73 Profound intellectual disabilities", PD, fixed=TRUE)
  PD <- gsub("46.3 Progressive multifocal leukoencephalopathy", "0A81.2 Progressive multifocal leukoencephalopathy", PD, fixed=TRUE)
  PD <- gsub("S71.132A Puncture wound without foreign body thigh", "S71.131A Puncture wound without foreign body thigh", PD, fixed=TRUE)
  PD <- gsub("272.0 Pure hypercholesterolemia", "E78.0 Pure hypercholesterolemia", PD, fixed=TRUE)
  PD <- gsub("E78.00 Pure hypercholesterolemia", "E78.0 Pure hypercholesterolemia", PD, fixed=TRUE)
  PD <- gsub("272.1 Pure hyperglyceridemia", "E78.1 Pure hyperglyceridemia", PD, fixed=TRUE)
  PD <- gsub("686.01 Pyoderma gangrenosum", "L88 Pyoderma gangrenosum", PD, fixed=TRUE)
  PD <- gsub("344.03 Quadriplegia", "G82.50 Quadriplegia", PD, fixed=TRUE)
  PD <- gsub("G82.54 Quadriplegia", "G82.50 Quadriplegia", PD, fixed=TRUE)
  PD <- gsub("344.00 Quadriplegia", "G82.50 Quadriplegia", PD, fixed=TRUE)
  PD <- gsub("G82.52 Quadriplegia", "G82.50 Quadriplegia", PD, fixed=TRUE)
  PD <- gsub("R21 Rash and other nonspecific skin eruption", "782.1 Rash and other nonspecific skin eruption", PD, fixed=TRUE)
  PD <- gsub("588.0 Renal osteodystrophy", "N25.0 Renal osteodystrophy", PD, fixed=TRUE)
  PD <- gsub("788.20 Retention of urine", "R33.9 Retention of urine", PD, fixed=TRUE)
  PD <- gsub("728.88 Rhabdomyolysis", "M62.82 Rhabdomyolysis", PD, fixed=TRUE)
  PD <- gsub("389.10 Sensorineural hearing loss", "H90.3 Sensorineural hearing loss", PD, fixed=TRUE)
  PD <- gsub("J85.3 Abscess of mediastinum", "513.1 Abscess of mediastinum", PD, fixed=TRUE)
  PD <- gsub("D68.4 Acquired coagulation factor deficiency", "286.7 Acquired coagulation factor deficiency", PD, fixed=TRUE)
  PD <- gsub("G93.1 Anoxic brain damage", "348.1 Anoxic brain damage", PD, fixed=TRUE)
  PD <- gsub("B44.9 Aspergillosis", "117.3 Aspergillosis", PD, fixed=TRUE)
  PD <- gsub("I44.2 Atrioventricular block", "426.0 Atrioventricular block", PD, fixed=TRUE)
  PD <- gsub("K75.4 Autoimmune hepatitis", "571.42 Autoimmune hepatitis", PD, fixed=TRUE)
  PD <- gsub("R64 Cachexia", "799.4 Cachexia", PD, fixed=TRUE)
  PD <- gsub("R57.0 Cardiogenic shock", "785.51 Cardiogenic shock", PD, fixed=TRUE)
  PD <- gsub("G93.6 Cerebral edema", "348.5 Cerebral edema", PD, fixed=TRUE)
  PD <- gsub("416.2 Chronic pulmonary embolism", "I27.82 Chronic pulmonary embolism", PD, fixed=TRUE)
  PD <- gsub("Z93.3 Colostomy status", "V44.3 Colostomy status", PD, fixed=TRUE)
  PD <- gsub("G93.5 Compression of brain", "348.4 Compression of brain", PD, fixed=TRUE)
  PD <- gsub("B25.9 Cytomegaloviral disease", "078.5 Cytomegaloviral disease", PD, fixed=TRUE)
  PD <- gsub("253.5 Diabetes insipidus", "E23.2 Diabetes insipidus", PD, fixed=TRUE)
  PD <- gsub("443.21 Dissection of carotid artery", "I77.71 Dissection of carotid artery", PD, fixed=TRUE)
  PD <- gsub("I74.5 Embolism and thrombosis of iliac artery", "444.81 Embolism and thrombosis of iliac artery", PD, fixed=TRUE)
  PD <- gsub("N18.6 End stage renal disease", "585.6 End stage renal disease", PD, fixed=TRUE)
  PD <- gsub("I96 Gangrene", "785.4 Gangrene", PD, fixed=TRUE)
  PD <- gsub("Z93.1 Gastrostomy status", "V44.1 Gastrostomy status", PD, fixed=TRUE)
  PD <- gsub("282.62 Hb-SS disease with crisis", "D57.00 Hb-SS disease with crisis", PD, fixed=TRUE)
  PD <- gsub("573.5 Hepatopulmonary syndrome", "K76.81 Hepatopulmonary syndrome", PD, fixed=TRUE)
  PD <- gsub("K76.7 Hepatorenal syndrome", "572.4 Hepatorenal syndrome", PD, fixed=TRUE)
  PD <- gsub("V44.2 Ileostomy status", "Z93.2 Ileostomy status", PD, fixed=TRUE)
  PD <- gsub("518.1 Interstitial emphysema", "J98.2 Interstitial emphysema", PD, fixed=TRUE)
  PD <- gsub("V49.74 Lower limb amputation", "V49.73 Lower limb amputation", PD, fixed=TRUE)
  PD <- gsub("296.30 Major depressive disorder episode", "296.32 Major depressive disorder episode", PD, fixed=TRUE)
  PD <- gsub("141.0 Malignant neoplasm of base of tongue", "C01 Malignant neoplasm of base of tongue", PD, fixed=TRUE)
  PD <- gsub("C25.0 Malignant neoplasm of head of pancreas", "157.0 Malignant neoplasm of head of pancreas", PD, fixed=TRUE)
  PD <- gsub("142.0 Malignant neoplasm of parotid gland", "C07 Malignant neoplasm of parotid gland", PD, fixed=TRUE)
  PD <- gsub("C61 Malignant neoplasm of prostate", "185 Malignant neoplasm of prostate", PD, fixed=TRUE)
  PD <- gsub("C20 Malignant neoplasm of rectum", "154.1 Malignant neoplasm of rectum", PD, fixed=TRUE)
  PD <- gsub("340 Multiple sclerosis", "G35 Multiple sclerosis", PD, fixed=TRUE)
  PD <- gsub("M72.6 Necrotizing fasciitis", "728.86 Necrotizing fasciitis", PD, fixed=TRUE)
  PD <- gsub("588.1 Nephrogenic diabetes insipidus", "N25.1 Nephrogenic diabetes insipidus", PD, fixed=TRUE)
  PD <- gsub("261 Nutritional marasmus", "E41 Nutritional marasmus", PD, fixed=TRUE)
  PD <- gsub("304.01 Opioid type dependence", "304.00 Opioid type dependence", PD, fixed=TRUE)
  PD <- gsub("284.12 Other drug-induced pancytopenia", "D61.811 Other drug-induced pancytopenia", PD, fixed=TRUE)
  PD <- gsub("J43.8 Other emphysema", "492.8 Other emphysema", PD, fixed=TRUE)
  PD <- gsub("D61.818 Other pancytopenia", "284.19 Other pancytopenia", PD, fixed=TRUE)
  PD <- gsub("J95.09 Other tracheostomy complication", "519.09 Other tracheostomy complication", PD, fixed=TRUE)
  PD <- gsub("K56.0 Paralytic ileus", "560.1 Paralytic ileus", PD, fixed=TRUE)
  PD <- gsub("G82.20 Paraplegia", "344.1 Paraplegia", PD, fixed=TRUE)
  PD <- gsub("K65.1 Peritoneal abscess", "567.22 Peritoneal abscess", PD, fixed=TRUE)
  PD <- gsub("780.03 Persistent vegetative state", "R40.3 Persistent vegetative state", PD, fixed=TRUE)
  PD <- gsub("J15.0 Pneumonia due to Klebsiella pneumoniae", "482.0 Pneumonia due to Klebsiella pneumoniae", PD, fixed=TRUE)
  PD <- gsub("J15.1 Pneumonia due to Pseudomonas", "482.1 Pneumonia due to Pseudomonas", PD, fixed=TRUE)
  PD <- gsub("J15.3 Pneumonia due to Streptococcus b", "482.32 Pneumonia due to Streptococcus b", PD, fixed=TRUE)
  PD <- gsub("J15.6 Pneumonia due to other gram-negative bacteria", "482.83 Pneumonia due to other gram-negative bacteria", PD, fixed=TRUE)
  PD <- gsub("K76.6 Portal hypertension", "572.3 Portal hypertension", PD, fixed=TRUE)
  PD <- gsub("00A81.2 Progressive multifocal leukoencephalopathy", "A81.2 Progressive multifocal leukoencephalopathy", PD, fixed=TRUE)
  PD <- gsub("G82.50 Quadriplegia", "344.04 Quadriplegia", PD, fixed=TRUE)
  PD <- gsub("D86.9 Sarcoidosis", "135 Sarcoidosis", PD, fixed=TRUE)
  PD <- gsub("C77.1 Secondary and unspecified malignant neoplasm of intrathoracic lymph nodes", "196.1 Secondary and unspecified malignant neoplasm of intrathoracic lymph nodes", PD, fixed=TRUE)
  PD <- gsub("C78.39 Secondary malignant neoplasm of other respiratory organs", "197.3 Secondary malignant neoplasm of other respiratory organs", PD, fixed=TRUE)
  PD <- gsub("C78.6 Secondary malignant neoplasm of retroperitoneum and peritoneum", "197.6 Secondary malignant neoplasm of retroperitoneum and peritoneum", PD, fixed=TRUE)
  PD <- gsub("K65.2 Spontaneous bacterial peritonitis", "567.23 Spontaneous bacterial peritonitis", PD, fixed=TRUE)
  PD <- gsub("M32.9 Systemic lupus erythematosus", "710.0 Systemic lupus erythematosus", PD, fixed=TRUE)
  PD <- gsub("Z93.0 Tracheostomy status", "V44.0 Tracheostomy status", PD, fixed=TRUE)
  PD <- gsub("T79.4XXA Traumatic shock", "958.4 Traumatic shock", PD, fixed=TRUE)
  PD <- gsub("T79.7XXA Traumatic subcutaneous emphysema", "958.7 Traumatic subcutaneous emphysema", PD, fixed=TRUE)
  PD <- gsub("730.20 Unspecified osteomyelitis", "730.24 Unspecified osteomyelitis", PD, fixed=TRUE)
  PD <- gsub("J95.851 Ventilator associated pneumonia", "997.31 Ventilator associated pneumonia", PD, fixed=TRUE)
  PD <- gsub("I49.01 Ventricular fibrillation", "427.41 Ventricular fibrillation", PD, fixed=TRUE)
  PD <- gsub("F72 Severe intellectual disabilities", "318.1 Severe intellectual disabilities", PD, fixed=TRUE)
  PD <- gsub("785.50 Shock", "R57.9 Shock", PD, fixed=TRUE)
  PD <- gsub("527.2 Sialoadenitis", "K11.20 Sialoadenitis", PD, fixed=TRUE)
  PD <- gsub("793.11 Solitary pulmonary nodule", "R91.1 Solitary pulmonary nodule", PD, fixed=TRUE)
  PD <- gsub("M48.06 Spinal stenosis region", "M48.02 Spinal stenosis region", PD, fixed=TRUE)
  PD <- gsub("789.2 Splenomegaly", "R16.1 Splenomegaly", PD, fixed=TRUE)
  PD <- gsub("M47.812 Spondylosis without myelopathy or radiculopathy region", "M47.816 Spondylosis without myelopathy or radiculopathy region", PD, fixed=TRUE)
  PD <- gsub("512.0 Spontaneous tension pneumothorax", "J93.0 Spontaneous tension pneumothorax", PD, fixed=TRUE)
  PD <- gsub("G00.3 Staphylococcal meningitis", "320.3 Staphylococcal meningitis", PD, fixed=TRUE)
  PD <- gsub("V45.88 Status post administration of tPA (rtPA) in a different facility within the last 24 hours prior to admission to current facility", "Z92.82 Status post administration of tPA (rtPA) in a different facility within the last 24 hours prior to admission to current facility", PD, fixed=TRUE)
  PD <- gsub("785.0 Tachycardia", "R00.0 Tachycardia", PD, fixed=TRUE)
  PD <- gsub("429.83 Takotsubo syndrome", "I51.81 Takotsubo syndrome", PD, fixed=TRUE)
  PD <- gsub("I71.1 Thoracic aortic aneurysm", "I71.2 Thoracic aortic aneurysm", PD, fixed=TRUE)
  PD <- gsub("287.5 Thrombocytopenia", "D69.6 Thrombocytopenia", PD, fixed=TRUE)
  PD <- gsub("349.82 Toxic encephalopathy", "G92 Toxic encephalopathy", PD, fixed=TRUE)
  PD <- gsub("K71.10 Toxic liver disease with hepatic necrosis coma", "K71.11 Toxic liver disease with hepatic necrosis coma", PD, fixed=TRUE)
  PD <- gsub("277.88 Tumor lysis syndrome", "E88.3 Tumor lysis syndrome", PD, fixed=TRUE)
  PD <- gsub("E11.3522 Type 2 diabetes mellitus with proliferative diabetic retinopathy with traction retinal detachment involving the macula eye", "E11.3521 Type 2 diabetes mellitus with proliferative diabetic retinopathy with traction retinal detachment involving the macula eye", PD, fixed=TRUE)
  PD <- gsub("569.41 Ulcer of anus and rectum", "K62.6 Ulcer of anus and rectum", PD, fixed=TRUE)
  PD <- gsub("M16.12 Unilateral primary osteoarthritis hip", "M16.10 Unilateral primary osteoarthritis hip", PD, fixed=TRUE)
  PD <- gsub("366.9 Unspecified cataract", "H26.9 Unspecified cataract", PD, fixed=TRUE)
  PD <- gsub("H91.93 Unspecified hearing loss", "389.9 Unspecified hearing loss", PD, fixed=TRUE)
  PD <- gsub("319 Unspecified intellectual disabilities", "F79 Unspecified intellectual disabilities", PD, fixed=TRUE)
  PD <- gsub("560.9 Unspecified intestinal obstruction", "K56.60 Unspecified intestinal obstruction", PD, fixed=TRUE)
  PD <- gsub("383.9 Unspecified mastoiditis", "H70.93 Unspecified mastoiditis", PD, fixed=TRUE)
  PD <- gsub("H70.90 Unspecified mastoiditis ear", "H70.91 Unspecified mastoiditis ear", PD, fixed=TRUE)
  PD <- gsub("263.9 Unspecified protein-calorie malnutrition", "E46 Unspecified protein-calorie malnutrition", PD, fixed=TRUE)
  PD <- gsub("788.30 Unspecified urinary incontinence", "R32 Unspecified urinary incontinence", PD, fixed=TRUE)
  PD <- gsub("070.70 Unspecified viral hepatitis C without hepatic coma", "B19.20 Unspecified viral hepatitis C without hepatic coma", PD, fixed=TRUE)
  PD <- gsub("599.4 Urethral false passage", "N36.5 Urethral false passage", PD, fixed=TRUE)
  PD <- gsub("599.0 Urinary tract infection not specified", "N39.0 Urinary tract infection not specified", PD, fixed=TRUE)
  PD <- gsub("S02.40EA Zygomatic fracture side for closed fracture", "S02.402A Zygomatic fracture side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("789.33 Abdominal or pelvic swelling lump lower quadrant", "789.34 Abdominal or pelvic swelling lump lower quadrant", PD, fixed=TRUE)
  PD <- gsub("789.03 Abdominal pain lower quadrant", "789.04 Abdominal pain lower quadrant", PD, fixed=TRUE)
  PD <- gsub("789.02 Abdominal pain upper quadrant", "789.01 Abdominal pain upper quadrant", PD, fixed=TRUE)
  PD <- gsub("S90.511A Abrasion ankle", "S90.512A Abrasion ankle", PD, fixed=TRUE)
  PD <- gsub("S80.212A Abrasion knee", "S80.211A Abrasion knee", PD, fixed=TRUE)
  PD <- gsub("Z90.721 Acquired absence of ovaries", "Z90.722 Acquired absence of ovaries", PD, fixed=TRUE)
  PD <- gsub("303.01 Acute alcoholic intoxication in alcoholism", "303.00 Acute alcoholic intoxication in alcoholism", PD, fixed=TRUE)
  PD <- gsub("K35.2 Acute appendicitis with generalized peritonitis", "540.0 Acute appendicitis with generalized peritonitis", PD, fixed=TRUE)
  PD <- gsub("462 Acute pharyngitis", "J02.9 Acute pharyngitis", PD, fixed=TRUE)
  PD <- gsub("T45.1X5A Adverse effect of antineoplastic and immunosuppressive drugs", "T45.1X5S Adverse effect of antineoplastic and immunosuppressive drugs", PD, fixed=TRUE)
  PD <- gsub("T50.8X5A Adverse effect of diagnostic agents", "T50.8X5S Adverse effect of diagnostic agents", PD, fixed=TRUE)
  PD <- gsub("F10.10 Alcohol abuse", "305.01 Alcohol abuse", PD, fixed=TRUE)
  PD <- gsub("F10.129 Alcohol abuse with intoxication", "F10.120 Alcohol abuse with intoxication", PD, fixed=TRUE)
  PD <- gsub("F10.220 Alcohol dependence with intoxication", "F10.229 Alcohol dependence with intoxication", PD, fixed=TRUE)
  PD <- gsub("F10.239 Alcohol dependence with withdrawal", "F10.230 Alcohol dependence with withdrawal", PD, fixed=TRUE)
  PD <- gsub("K70.0 Alcoholic fatty liver", "571.0 Alcoholic fatty liver", PD, fixed=TRUE)
  PD <- gsub("708.0 Allergic urticaria", "L50.0 Allergic urticaria", PD, fixed=TRUE)
  PD <- gsub("Z91.011 Allergy to milk products", "V15.02 Allergy to milk products", PD, fixed=TRUE)
  PD <- gsub("Z91.010 Allergy to peanuts", "V15.01 Allergy to peanuts", PD, fixed=TRUE)
  PD <- gsub("304.41 Amphetamine and other psychostimulant dependence", "304.40 Amphetamine and other psychostimulant dependence", PD, fixed=TRUE)
  PD <- gsub("I70.0 Atherosclerosis of aorta", "440.0 Atherosclerosis of aorta", PD, fixed=TRUE)
  PD <- gsub("225.2 Benign neoplasm of cerebral meninges", "D32.0 Benign neoplasm of cerebral meninges", PD, fixed=TRUE)
  PD <- gsub("D33.3 Benign neoplasm of cranial nerves", "225.1 Benign neoplasm of cranial nerves", PD, fixed=TRUE)
  PD <- gsub("D16.5 Benign neoplasm of lower jaw bone", "213.1 Benign neoplasm of lower jaw bone", PD, fixed=TRUE)
  PD <- gsub("Z68.27 Body mass index 27.0-27.9", "V85.23 Body mass index 27.0-27.9", PD, fixed=TRUE)
  PD <- gsub("F60.3 Borderline personality disorder", "301.83 Borderline personality disorder", PD, fixed=TRUE)
  PD <- gsub("N20.1 Calculus of ureter", "592.1 Calculus of ureter", PD, fixed=TRUE)
  PD <- gsub("429.3 Cardiomegaly", "I51.7 Cardiomegaly", PD, fixed=TRUE)
  PD <- gsub("G93.0 Cerebral cysts", "348.0 Cerebral cysts", PD, fixed=TRUE)
  PD <- gsub("K05.32 Chronic periodontitis", "523.40 Chronic periodontitis", PD, fixed=TRUE)
  PD <- gsub("850.5 Concussion with loss of consciousness of unspecified duration", "S06.0X9A Concussion with loss of consciousness of unspecified duration", PD, fixed=TRUE)
  PD <- gsub("Q03.9 Congenital hydrocephalus", "742.3 Congenital hydrocephalus", PD, fixed=TRUE)
  PD <- gsub("M24.541 Contracture hand", "M24.542 Contracture hand", PD, fixed=TRUE)
  PD <- gsub("E24.9 Cushing's syndrome", "255.0 Cushing's syndrome", PD, fixed=TRUE)
  PD <- gsub("S06.2X9S Diffuse traumatic brain injury with loss of consciousness of unspecified duration", "S06.2X9A Diffuse traumatic brain injury with loss of consciousness of unspecified duration", PD, fixed=TRUE)
  PD <- gsub("H53.2 Diplopia", "368.2 Diplopia", PD, fixed=TRUE)
  PD <- gsub("I77.74 Dissection of vertebral artery", "443.24 Dissection of vertebral artery", PD, fixed=TRUE)
  PD <- gsub("780.4 Dizziness and giddiness", "R42 Dizziness and giddiness", PD, fixed=TRUE)
  PD <- gsub("788.1 Dysuria", "R30.0 Dysuria", PD, fixed=TRUE)
  PD <- gsub("444.9 Embolism and thrombosis of unspecified artery", "I74.9 Embolism and thrombosis of unspecified artery", PD, fixed=TRUE)
  PD <- gsub("R45.86 Emotional lability", "799.24 Emotional lability", PD, fixed=TRUE)
  PD <- gsub("Z17.0 Estrogen receptor positive status (ER+)", "V86.0 Estrogen receptor positive status (ER+)", PD, fixed=TRUE)
  PD <- gsub("Z83.3 Family history of diabetes mellitus", "V18.0 Family history of diabetes mellitus", PD, fixed=TRUE)
  PD <- gsub("V16.3 Family history of malignant neoplasm of breast", "Z80.3 Family history of malignant neoplasm of breast", PD, fixed=TRUE)
  PD <- gsub("Z80.51 Family history of malignant neoplasm of kidney", "V16.51 Family history of malignant neoplasm of kidney", PD, fixed=TRUE)
  PD <- gsub("Z80.42 Family history of malignant neoplasm of prostate", "V16.42 Family history of malignant neoplasm of prostate", PD, fixed=TRUE)
  PD <- gsub("M21.372 Foot drop foot", "M21.371 Foot drop foot", PD, fixed=TRUE)
  PD <- gsub("S22.31XA Fracture of one rib side for closed fracture", "S22.32XA Fracture of one rib side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("S02.31XA Fracture of orbital floor side for closed fracture", "S02.32XA Fracture of orbital floor side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("S02.81XA Fracture of other specified skull and facial bones side for closed fracture", "S02.82XA Fracture of other specified skull and facial bones side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("K29.70 Gastritis bleeding", "K29.71 Gastritis bleeding", PD, fixed=TRUE)
  PD <- gsub("300.02 Generalized anxiety disorder", "F41.1 Generalized anxiety disorder", PD, fixed=TRUE)
  PD <- gsub("I51.9 Heart disease", "429.9 Heart disease", PD, fixed=TRUE)
  PD <- gsub("786.8 Hiccough", "R06.6 Hiccough", PD, fixed=TRUE)
  PD <- gsub("H53.461 Homonymous bilateral field defects side", "H53.462 Homonymous bilateral field defects side", PD, fixed=TRUE)
  PD <- gsub("I67.4 Hypertensive encephalopathy", "437.2 Hypertensive encephalopathy", PD, fixed=TRUE)
  PD <- gsub("R06.4 Hyperventilation", "786.01 Hyperventilation", PD, fixed=TRUE)
  PD <- gsub("380.4 Impacted cerumen", "H61.23 Impacted cerumen", PD, fixed=TRUE)
  PD <- gsub("H47.012 Ischemic optic neuropathy eye", "H47.011 Ischemic optic neuropathy eye", PD, fixed=TRUE)
  PD <- gsub("R68.84 Jaw pain", "784.92 Jaw pain", PD, fixed=TRUE)
  PD <- gsub("R22.0 Localized swelling and lump", "R22.1 Localized swelling and lump", PD, fixed=TRUE)
  PD <- gsub("C31.9 Malignant neoplasm of accessory sinus", "160.9 Malignant neoplasm of accessory sinus", PD, fixed=TRUE)
  PD <- gsub("C18.2 Malignant neoplasm of ascending colon", "153.6 Malignant neoplasm of ascending colon", PD, fixed=TRUE)
  PD <- gsub("C53.0 Malignant neoplasm of endocervix", "180.0 Malignant neoplasm of endocervix", PD, fixed=TRUE)
  PD <- gsub("C34.32 Malignant neoplasm of lower lobe bronchus or lung", "C34.31 Malignant neoplasm of lower lobe bronchus or lung", PD, fixed=TRUE)
  PD <- gsub("C15.5 Malignant neoplasm of lower third of esophagus", "150.5 Malignant neoplasm of lower third of esophagus", PD, fixed=TRUE)
  PD <- gsub("C19 Malignant neoplasm of rectosigmoid junction", "154.0 Malignant neoplasm of rectosigmoid junction", PD, fixed=TRUE)
  PD <- gsub("C73 Malignant neoplasm of thyroid gland", "193 Malignant neoplasm of thyroid gland", PD, fixed=TRUE)
  PD <- gsub("M26.213 Malocclusion", "M26.4 Malocclusion", PD, fixed=TRUE)
  PD <- gsub("524.4 Malocclusion", "M26.4 Malocclusion", PD, fixed=TRUE)
  PD <- gsub("524.22 Malocclusion", "M26.4 Malocclusion", PD, fixed=TRUE)
  PD <- gsub("524.23 Malocclusion", "M26.4 Malocclusion", PD, fixed=TRUE)
  PD <- gsub("524.02 Mandibular hyperplasia", "M26.03 Mandibular hyperplasia", PD, fixed=TRUE)
  PD <- gsub("M26.04 Mandibular hypoplasia", "M26.03 Mandibular hyperplasia", PD, fixed=TRUE)
  PD <- gsub("524.04 Mandibular hypoplasia", "M26.03 Mandibular hyperplasia", PD, fixed=TRUE)
  PD <- gsub("759.82 Marfan's syndrome", "Q87.40 Marfan's syndrome", PD, fixed=TRUE)
  PD <- gsub("S02.401A Maxillary fracture side for closed fracture", "S02.40DA Maxillary fracture side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("M26.01 Maxillary hyperplasia", "524.01 Maxillary hyperplasia", PD, fixed=TRUE)
  PD <- gsub("M26.02 Maxillary hypoplasia", "524.03 Maxillary hypoplasia", PD, fixed=TRUE)
  PD <- gsub("G31.84 Mild cognitive impairment stated", "331.83 Mild cognitive impairment stated", PD, fixed=TRUE)
  PD <- gsub("S22.41XD Multiple fractures of ribs side encounter for fracture with routine healing", "S22.42XD Multiple fractures of ribs side encounter for fracture with routine healing", PD, fixed=TRUE)
  PD <- gsub("R35.1 Nocturia", "788.43 Nocturia", PD, fixed=TRUE)
  PD <- gsub("E04.2 Nontoxic multinodular goiter", "241.1 Nontoxic multinodular goiter", PD, fixed=TRUE)
  PD <- gsub("I61.1 Nontraumatic intracerebral hemorrhage in hemisphere", "I61.2 Nontraumatic intracerebral hemorrhage in hemisphere", PD, fixed=TRUE)
  PD <- gsub("305.50 Opioid abuse", "F11.10 Opioid abuse", PD, fixed=TRUE)
  PD <- gsub("H92.09 Otalgia ear", "H92.01 Otalgia ear", PD, fixed=TRUE)
  PD <- gsub("I74.09 Other arterial embolism and thrombosis of abdominal aorta", "444.09 Other arterial embolism and thrombosis of abdominal aorta", PD, fixed=TRUE)
  PD <- gsub("K95.89 Other complications of other bariatric procedure", "539.89 Other complications of other bariatric procedure", PD, fixed=TRUE)
  PD <- gsub("J38.7 Other diseases of larynx", "478.79 Other diseases of larynx", PD, fixed=TRUE)
  PD <- gsub("J98.59 Other diseases of mediastinum", "519.3 Other diseases of mediastinum", PD, fixed=TRUE)
  PD <- gsub("H55.09 Other forms of nystagmus", "379.56 Other forms of nystagmus", PD, fixed=TRUE)
  PD <- gsub("I42.2 Other hypertrophic cardiomyopathy", "425.18 Other hypertrophic cardiomyopathy", PD, fixed=TRUE)
  PD <- gsub("M60.811 Other myositis shoulder", "M60.812 Other myositis shoulder", PD, fixed=TRUE)
  PD <- gsub("H46.8 Other optic neuritis", "377.39 Other optic neuritis", PD, fixed=TRUE)
  PD <- gsub("M87.851 Other osteonecrosis femur", "M87.852 Other osteonecrosis femur", PD, fixed=TRUE)
  PD <- gsub("L82.1 Other seborrheic keratosis", "702.19 Other seborrheic keratosis", PD, fixed=TRUE)
  PD <- gsub("D64.89 Other specified anemias", "285.8 Other specified anemias", PD, fixed=TRUE)
  PD <- gsub("Z71.89 Other specified counseling", "V65.49 Other specified counseling", PD, fixed=TRUE)
  PD <- gsub("S85.092A Other specified injury of popliteal artery leg", "S85.091A Other specified injury of popliteal artery leg", PD, fixed=TRUE)
  PD <- gsub("M47.26 Other spondylosis with radiculopathy region", "M47.22 Other spondylosis with radiculopathy region", PD, fixed=TRUE)
  PD <- gsub("F15.129 Other stimulant abuse with intoxication", "F15.120 Other stimulant abuse with intoxication", PD, fixed=TRUE)
  PD <- gsub("N39.490 Overflow incontinence", "788.38 Overflow incontinence", PD, fixed=TRUE)
  PD <- gsub("378.52 Paralytic strabismus", "378.51 Paralytic strabismus", PD, fixed=TRUE)
  PD <- gsub("295.30 Paranoid schizophrenia condition", "295.32 Paranoid schizophrenia condition", PD, fixed=TRUE)
  PD <- gsub("344.1 Paraplegia", "G82.21 Paraplegia", PD, fixed=TRUE)
  PD <- gsub("I47.9 Paroxysmal tachycardia", "427.2 Paroxysmal tachycardia", PD, fixed=TRUE)
  PD <- gsub("Z87.74 Personal history of (corrected) congenital malformations of heart and circulatory system", "V13.65 Personal history of (corrected) congenital malformations of heart and circulatory system", PD, fixed=TRUE)
  PD <- gsub("Z86.010 Personal history of colonic polyps", "V12.72 Personal history of colonic polyps", PD, fixed=TRUE)
  PD <- gsub("Z86.61 Personal history of infections of the central nervous system", "V12.42 Personal history of infections of the central nervous system", PD, fixed=TRUE)
  PD <- gsub("Z85.820 Personal history of malignant melanoma of skin", "V10.82 Personal history of malignant melanoma of skin", PD, fixed=TRUE)
  PD <- gsub("Z85.51 Personal history of malignant neoplasm of bladder", "V10.51 Personal history of malignant neoplasm of bladder", PD, fixed=TRUE)
  PD <- gsub("Z85.841 Personal history of malignant neoplasm of brain", "V10.85 Personal history of malignant neoplasm of brain", PD, fixed=TRUE)
  PD <- gsub("V10.42 Personal history of malignant neoplasm of other parts of uterus", "Z85.42 Personal history of malignant neoplasm of other parts of uterus", PD, fixed=TRUE)
  PD <- gsub("Z87.01 Personal history of pneumonia (recurrent", "V12.61 Personal history of pneumonia (recurrent", PD, fixed=TRUE)
  PD <- gsub("Z86.11 Personal history of tuberculosis", "V12.01 Personal history of tuberculosis", PD, fixed=TRUE)
  PD <- gsub("M35.3 Polymyalgia rheumatica", "725 Polymyalgia rheumatica", PD, fixed=TRUE)
  PD <- gsub("R56.1 Post traumatic seizures", "780.33 Post traumatic seizures", PD, fixed=TRUE)
  PD <- gsub("F43.10 Post-traumatic stress disorder", "F43.12 Post-traumatic stress disorder", PD, fixed=TRUE)
  PD <- gsub("722.80 Postlaminectomy syndrome region", "722.83 Postlaminectomy syndrome region", PD, fixed=TRUE)
  PD <- gsub("R50.82 Postprocedural fever", "780.62 Postprocedural fever", PD, fixed=TRUE)
  PD <- gsub("786.51 Precordial pain", "R07.2 Precordial pain", PD, fixed=TRUE)
  PD <- gsub("M19.071 Primary osteoarthritis ankle and foot", "M19.072 Primary osteoarthritis ankle and foot", PD, fixed=TRUE)
  PD <- gsub("M19.041 Primary osteoarthritis hand", "M19.042 Primary osteoarthritis hand", PD, fixed=TRUE)
  PD <- gsub("R80.9 Proteinuria", "791.0 Proteinuria", PD, fixed=TRUE)
  PD <- gsub("S31.141A Puncture wound of abdominal wall with foreign body upper quadrant without penetration into peritoneal cavity", "S31.140A Puncture wound of abdominal wall with foreign body upper quadrant without penetration into peritoneal cavity", PD, fixed=TRUE)
  PD <- gsub("S71.142A Puncture wound with foreign body thigh", "S71.141A Puncture wound with foreign body thigh", PD, fixed=TRUE)
  PD <- gsub("711.03 Pyogenic arthritis", "711.04 Pyogenic arthritis", PD, fixed=TRUE)
  PD <- gsub("D69.1 Qualitative platelet defects", "287.1 Qualitative platelet defects", PD, fixed=TRUE)
  PD <- gsub("K64.4 Residual hemorrhoidal skin tags", "455.9 Residual hemorrhoidal skin tags", PD, fixed=TRUE)
  PD <- gsub("Z18.10 Retained metal fragments", "V90.10 Retained metal fragments", PD, fixed=TRUE)
  PD <- gsub("M54.30 Sciatica side", "M54.32 Sciatica side", PD, fixed=TRUE)
  PD <- gsub("C79.49 Secondary malignant neoplasm of other parts of nervous system", "198.4 Secondary malignant neoplasm of other parts of nervous system", PD, fixed=TRUE)
  PD <- gsub("C78.2 Secondary malignant neoplasm of pleura", "197.2 Secondary malignant neoplasm of pleura", PD, fixed=TRUE)
  PD <- gsub("H90.3 Sensorineural hearing loss", "389.15 Sensorineural hearing loss", PD, fixed=TRUE)
  PD <- gsub("710.2 Sicca syndrome", "M35.00 Sicca syndrome", PD, fixed=TRUE)
  PD <- gsub("282.5 Sickle-cell trait", "D57.3 Sickle-cell trait", PD, fixed=TRUE)
  PD <- gsub("H49.22 Sixth (abducent) nerve palsy eye", "H49.21 Sixth (abducent) nerve palsy eye", PD, fixed=TRUE)
  PD <- gsub("741.00 Spina bifida with hydrocephalus region", "741.03 Spina bifida with hydrocephalus region", PD, fixed=TRUE)
  PD <- gsub("724.02 Spinal stenosis region neurogenic claudication", "724.03 Spinal stenosis region neurogenic claudication", PD, fixed=TRUE)
  PD <- gsub("J38.6 Stenosis of larynx", "478.74 Stenosis of larynx", PD, fixed=TRUE)
  PD <- gsub("695.13 Stevens-Johnson syndrome", "L51.1 Stevens-Johnson syndrome", PD, fixed=TRUE)
  PD <- gsub("G00.2 Streptococcal meningitis", "320.2 Streptococcal meningitis", PD, fixed=TRUE)
  PD <- gsub("780.2 Syncope and collapse", "R55 Syncope and collapse", PD, fixed=TRUE)
  PD <- gsub("M34.9 Systemic sclerosis", "710.1 Systemic sclerosis", PD, fixed=TRUE)
  PD <- gsub("R06.82 Tachypnea", "786.06 Tachypnea", PD, fixed=TRUE)
  PD <- gsub("G93.81 Temporal sclerosis", "348.81 Temporal sclerosis", PD, fixed=TRUE)
  PD <- gsub("780.02 Transient alteration of awareness", "780.02 Transient alteration of awareness", PD, fixed=TRUE)
  PD <- gsub("R40.4 Transient alteration of awareness", "780.02 Transient alteration of awareness", PD, fixed=TRUE)
  PD <- gsub("Z98.85 Transplanted organ removal status", "V45.87 Transplanted organ removal status", PD, fixed=TRUE)
  PD <- gsub("G50.0 Trigeminal neuralgia", "350.1 Trigeminal neuralgia", PD, fixed=TRUE)
  PD <- gsub("Z98.51 Tubal ligation status", "V26.51 Tubal ligation status", PD, fixed=TRUE)
  PD <- gsub("F45.1 Undifferentiated somatoform disorder", "300.82 Undifferentiated somatoform disorder", PD, fixed=TRUE)
  PD <- gsub("M17.11 Unilateral primary osteoarthritis knee", "M17.12 Unilateral primary osteoarthritis knee", PD, fixed=TRUE)
  PD <- gsub("I70.201 Unspecified atherosclerosis of native arteries of extremities leg", "I70.202 Unspecified atherosclerosis of native arteries of extremities leg", PD, fixed=TRUE)
  PD <- gsub("304.90 Unspecified drug dependence", "304.91 Unspecified drug dependence", PD, fixed=TRUE)
  PD <- gsub("H91.90 Unspecified hearing loss ear", "H91.91 Unspecified hearing loss ear", PD, fixed=TRUE)
  PD <- gsub("S06.9X0A Unspecified intracranial injury without loss of consciousness", "S06.9X0S Unspecified intracranial injury without loss of consciousness", PD, fixed=TRUE)
  PD <- gsub("M40.202 Unspecified kyphosis region", "M40.205 Unspecified kyphosis region", PD, fixed=TRUE)
  PD <- gsub("H53.9 Unspecified visual disturbance", "368.9 Unspecified visual disturbance", PD, fixed=TRUE)
  PD <- gsub("H54.7 Unspecified visual loss", "369.9 Unspecified visual loss", PD, fixed=TRUE)
  PD <- gsub("N39.41 Urge incontinence", "788.31 Urge incontinence", PD, fixed=TRUE)
  PD <- gsub("R39.15 Urgency of urination", "788.63 Urgency of urination", PD, fixed=TRUE)
  PD <- gsub("618.3 Uterovaginal prolapse", "618.4 Uterovaginal prolapse", PD, fixed=TRUE)
  PD <- gsub("Q21.0 Ventricular septal defect", "745.4 Ventricular septal defect", PD, fixed=TRUE)
  PD <- gsub("K56.2 Volvulus", "560.2 Volvulus", PD, fixed=TRUE)
  PD <- gsub("786.07 Wheezing", "R06.2 Wheezing", PD, fixed=TRUE)
  PD <- gsub("S02.40FA Zygomatic fracture side for closed fracture", "S02.402A Zygomatic fracture side for closed fracture", PD, fixed=TRUE)
  PD <- gsub("S85.012A Laceration of popliteal artery leg", "S85.011A Laceration of popliteal artery leg", PD, fixed=TRUE)
  PD <- gsub("S81.811A Laceration without foreign body lower leg", "S81.819A Laceration without foreign body lower leg", PD, fixed=TRUE)
  PD <- gsub("G40.001 Localization-related (focal) (partial) idiopathic epilepsy and epileptic syndromes with seizures of localized onset intractable status epilepticus", "G40.009 Localization-related (focal) (partial) idiopathic epilepsy and epileptic syndromes with seizures of localized onset intractable status epilepticus", PD, fixed=TRUE)
  PD <- gsub("G40.019 Localization-related (focal) (partial) idiopathic epilepsy and epileptic syndromes with seizures of localized onset status epilepticus", "G40.011 Localization-related (focal) (partial) idiopathic epilepsy and epileptic syndromes with seizures of localized onset status epilepticus", PD, fixed=TRUE)
  PD <- gsub("G40.219 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with complex partial seizures status epilepticus", "G40.211 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with complex partial seizures status epilepticus", PD, fixed=TRUE)
  PD <- gsub("G40.109 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with simple partial seizures intractable status epilepticus", "G40.101 Localization-related (focal) (partial) symptomatic epilepsy and epileptic syndromes with simple partial seizures intractable status epilepticus", PD, fixed=TRUE)
  PD <- gsub("I50.31 Acute diastolic (congestive) heart failure", "50.31 Acute diastolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("I82.611 Acute embolism and thrombosis of superficial veins of right upper extremity", "82.611 Acute embolism and thrombosis of superficial veins of right upper extremity", PD, fixed=TRUE)
  PD <- gsub("584.9 Acute kidney failure", "17.9 Acute kidney failure", PD, fixed=TRUE)
  PD <- gsub("I50.33 Acute on chronic diastolic (congestive) heart failure", "50.33 Acute on chronic diastolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("I50.23 Acute on chronic systolic (congestive) heart failure", "50.23 Acute on chronic systolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("730.07 Acute osteomyelitis and foot", "30.07 Acute osteomyelitis and foot", PD, fixed=TRUE)
  PD <- gsub("J96.00 Acute respiratory failure whether with hypoxia or hypercapnia", "96.00 Acute respiratory failure whether with hypoxia or hypercapnia", PD, fixed=TRUE)
  PD <- gsub("I50.21 Acute systolic (congestive) heart failure", "50.21 Acute systolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("00.00 Anxiety state", "300.00 Anxiety state", PD, fixed=TRUE)
  PD <- gsub("R78.81 Bacteremia", "90.7 Bacteremia", PD, fixed=TRUE)
  PD <- gsub("721.0 Cervical spondylosis without myelopathy", "21.0 Cervical spondylosis without myelopathy", PD, fixed=TRUE)
  PD <- gsub("I48.2 Chronic atrial fibrillation", "48.2 Chronic atrial fibrillation", PD, fixed=TRUE)
  PD <- gsub("I50.32 Chronic diastolic (congestive) heart failure", "50.32 Chronic diastolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("I50.22 Chronic systolic (congestive) heart failure", "50.22 Chronic systolic (congestive) heart failure", PD, fixed=TRUE)
  PD <- gsub("414.01 Coronary atherosclerosis of native coronary artery", "14.01 Coronary atherosclerosis of native coronary artery", PD, fixed=TRUE)
  PD <- gsub("799.3 Debility", "99.3 Debility", PD, fixed=TRUE)
  PD <- gsub("294.20 Dementia behavior", "294.21 Dementia behavior", PD, fixed=TRUE)
  PD <- gsub("Z66 Do not resuscitate", "66 Do not resuscitate", PD, fixed=TRUE)
  PD <- gsub("R60.9 Edema", "82.3 Edema", PD, fixed=TRUE)
  PD <- gsub("G93.40 Encephalopathy", "93.40 Encephalopathy", PD, fixed=TRUE)
  PD <- gsub("276.7 Hyperpotassemia", "76.7 Hyperpotassemia", PD, fixed=TRUE)
  PD <- gsub("276.1 Hyposmolality and/or hyponatremia", "76.1 Hyposmolality and/or hyponatremia", PD, fixed=TRUE)
  PD <- gsub("R09.02 Hypoxemia", "09.02 Hypoxemia", PD, fixed=TRUE)
  PD <- gsub("K56.7 Ileus", "56.7 Ileus", PD, fixed=TRUE)
  PD <- gsub("Z79.01 Long term (current) use of anticoagulants", "79.01 Long term (current) use of anticoagulants", PD, fixed=TRUE)
  PD <- gsub("403.01 Malignant hypertensive kidney disease with chronic kidney disease stage V or end stage renal disease", "03.01 Malignant hypertensive kidney disease with chronic kidney disease stage V or end stage renal disease", PD, fixed=TRUE)
  PD <- gsub("84.0 Malignant neoplasm of vagina", "C52 Malignant neoplasm of vagina", PD, fixed=TRUE)
  PD <- gsub("1C52 Malignant neoplasm of vagina", "C52 Malignant neoplasm of vagina", PD, fixed=TRUE)
  PD <- gsub("728.86 Necrotizing fasciitis", "72.6 Necrotizing fasciitis", PD, fixed=TRUE)
  PD <- gsub("67.6 Nonpyogenic thrombosis of intracranial venous system", "I67.6 Nonpyogenic thrombosis of intracranial venous system", PD, fixed=TRUE)
  PD <- gsub("I25.2 Old myocardial infarction", "25.2 Old myocardial infarction", PD, fixed=TRUE)
  PD <- gsub("45.5 Ostium secundum type atrial septal defect", "745.5 Ostium secundum type atrial septal defect", PD, fixed=TRUE)
  PD <- gsub("415.19 Other pulmonary embolism and infarction", "15.19 Other pulmonary embolism and infarction", PD, fixed=TRUE)
  PD <- gsub("427.89 Other specified cardiac dysrhythmias(427.89)", "27.89 Other specified cardiac dysrhythmias(427.89)", PD, fixed=TRUE)
  PD <- gsub("511.89 Other specified forms of effusion tuberculous", "11.89 Other specified forms of effusion tuberculous", PD, fixed=TRUE)
  PD <- gsub("I48.0 Paroxysmal atrial fibrillation", "48.0 Paroxysmal atrial fibrillation", PD, fixed=TRUE)
  PD <- gsub("M84.58XA Pathological fracture in neoplastic disease specified site for fracture", "84.58XA Pathological fracture in neoplastic disease specified site for fracture", PD, fixed=TRUE)
  PD <- gsub("I73.9 Peripheral vascular disease", "73.9 Peripheral vascular disease", PD, fixed=TRUE)
  PD <- gsub("482.0 Pneumonia due to Klebsiella pneumoniae", "15.0 Pneumonia due to Klebsiella pneumoniae", PD, fixed=TRUE)
  PD <- gsub("507.0 Pneumonitis due to inhalation of food or vomitus", "07.0 Pneumonitis due to inhalation of food or vomitus", PD, fixed=TRUE)
  PD <- gsub("28.88 Rhabdomyolysis", "M62.82 Rhabdomyolysis", PD, fixed=TRUE)
  PD <- gsub("62.82 Rhabdomyolysis", "M62.82 Rhabdomyolysis", PD, fixed=TRUE)
  PD <- gsub("M54.32 Sciatica side", "54.30 Sciatica side", PD, fixed=TRUE)
  PD <- gsub("41.9 Scoliosis", "M41.9 Scoliosis", PD, fixed=TRUE)
  PD <- gsub("98.82 Secondary malignant neoplasm of genital organs", "198.82 Secondary malignant neoplasm of genital organs", PD, fixed=TRUE)
  PD <- gsub("98.4 Secondary malignant neoplasm of other parts of nervous system", "198.4 Secondary malignant neoplasm of other parts of nervous system", PD, fixed=TRUE)
  PD <- gsub("I47.1 Supraventricular tachycardia", "47.1 Supraventricular tachycardia", PD, fixed=TRUE)
  PD <- gsub("I48.91 Unspecified atrial fibrillation", "48.91 Unspecified atrial fibrillation", PD, fixed=TRUE)
  PD <- gsub("401.9 Unspecified essential hypertension", "01.9 Unspecified essential hypertension", PD, fixed=TRUE)
  PD <- gsub("404.91 Unspecified hypertensive heart and kidney disease with heart failure and with chronic kidney disease stage I through stage IV unspecified(404.91)", "04.91 Unspecified hypertensive heart and kidney disease with heart failure and with chronic kidney disease stage I through stage IV unspecified(404.91)", PD, fixed=TRUE)
  PD <- gsub("403.90 Unspecified hypertensive kidney disease with chronic kidney disease stage I through stage IV unspecified(403.90)", "03.90 Unspecified hypertensive kidney disease with chronic kidney disease stage I through stage IV unspecified(403.90)", PD, fixed=TRUE)
  PD <- gsub("403.91 Unspecified hypertensive kidney disease with chronic kidney disease stage V or end stage renal disease(403.91)", "03.91 Unspecified hypertensive kidney disease with chronic kidney disease stage V or end stage renal disease(403.91)", PD, fixed=TRUE)
  PD <- gsub("244.9 Unspecified hypothyroidism", "44.9 Unspecified hypothyroidism", PD, fixed=TRUE)
  PD <- gsub("N39.0 Urinary tract infection not specified", "99.0 Urinary tract infection not specified", PD, fixed=TRUE)
  PD <- gsub("427.41 Ventricular fibrillation", "49.01 Ventricular fibrillation", PD, fixed=TRUE)
  PD <- gsub("I49.3 Ventricular premature depolarization", "49.3 Ventricular premature depolarization", PD, fixed=TRUE)
  PD <- gsub("I47.2 Ventricular tachycardia", "47.2 Ventricular tachycardia", PD, fixed=TRUE)
  PD <- gsub("56.2 Volvulus", "560.2 Volvulus", PD, fixed=TRUE)
  PD <- gsub("0A41.9 Sepsis", "A41.9 Sepsis", PD, fixed=TRUE)
  PD <- gsub("238.0 Neoplasm of uncertain behavior", "D44.7 Neoplasm of uncertain behavior", PD, fixed=TRUE)
  PD <- gsub("237.0 Neoplasm of uncertain behavior", "D44.7 Neoplasm of uncertain behavior", PD, fixed=TRUE)
  PD <- gsub("238.6 Neoplasm of uncertain behavior", "D44.7 Neoplasm of uncertain behavior", PD, fixed=TRUE)
  PD <- gsub("237.2 Neoplasm of uncertain behavior", "D44.7 Neoplasm of uncertain behavior", PD, fixed=TRUE)
  PD <- gsub("D49.7 Neoplasm of unspecified behavior", "D49.6 Neoplasm of unspecified behavior", PD, fixed=TRUE)


  diag2 <- NULL
  for(a in 1:length(PD)){
    SP <- strsplit(PD[a], split = ",")
    SP <- trimws(SP[[1]])
    SP <- strsplit(SP, split = " ")

    if(length(SP) > 0){
      diag <- NULL
      annot2 <- NULL
      for(i in 1:length(SP)){
        # if(length(SP[[i]]) >0){
        diag[i] <- SP[[i]][1]
        annot <- SP[[i]][2:length(SP[[i]])]
        annot2[i] <- paste(annot, collapse = " ")
        # }
      }
      dt <- data.table(cbind(diag, annot2, DT$recno[a]))
      setnames(dt, c("diag", "annot2", "V3"), c("diagnosis", "Annotation", "recno"))
      diag2 <- rbind(diag2, dt)
    }
    else{
      dt <- data.table(cbind(NA, NA, DT$recno[a]))
      setnames(dt, c("V1", "V2", "V3"), c("diagnosis", "Annotation", "recno"))
      diag2 <- rbind(diag2, dt)
    }

  }
  diag2$diagnosis <- trimws(diag2$diagnosis)
  return(diag2)
}







#' ProcedureCleaner
#'
#' cleans the OR procedures into a usable format.
#'
#' @param  DT a data table containing a column named "patientdiagnosis" and a column named "recno" containing the record for each record.
#' @return a data table containing one column labeled "orprocedures" and a second colum labeled "recno" containing the initial record number the diagnosis originated from.
#' @author Brendan Gongol
#' @export

ProcedureCleaner <- function(DT){
  PD <- DT$`orprocedures`
  PD <- gsub("DISSECTION, NECK", "NECK_DISSECTION,", PD)
  PD <- gsub("LARYNGECTOMY PHARYNGECTOMY", "LARYNGECTOMY, PHARYNGECTOMY", PD)
  PD <- gsub("AMPUTATION, LOWER EXTREMITY, ABOVE OR BELOW KNEE", "LOWER_EXTREMITY_AMPUTATION", PD)
  PD <- gsub("LAPAROTOMY, EXPLORATORY", "EXPLORATORY_LAPROTOMY", PD)
  PD <- gsub("FASCIOTOMY, UPPER EXTREMITY", "FASCIOTOMY", PD)
  PD <- gsub("PELVIS EXPLORATORY_LAPROTOMY", "EXPLORATORY_LAPROTOMY", PD)
  PD <- gsub("EMERGENT FOR ABDOMINAL TRAUMA", "", PD)
  PD <- gsub("CREATION", "", PD)
  PD <- gsub("OPEN", "", PD)
  PD <- gsub("POSTERIOR APPROACH", "", PD)
  PD <- gsub("CERVICAL", "", PD)
  PD <- gsub("FUSION, SPINE", "SPINE_FUSION", PD)
  PD <- gsub("ANEURYSM, ABDOMINAL AORTA, WITH GRAFT", "", PD)
  PD <- gsub("REPLACEMENT, OR REMOVAL, SHUNT", "SHUNT_REPLACEMENT_OR_REMOVAL", PD)
  PD <- gsub("VENTRICULOPERITONEAL", "", PD)
  PD <- gsub("INTRACRANIAL HEMORRHAGE OR SUBDURAL HEMATOMA", "", PD)
  PD <- gsub("CRANIECTOMY OR CRANIOTOMY, EVACUATION", "CRANIECTOMY_OR_CRANIOTOMY_EVACUATION", PD)
  PD <- gsub("EXPLORATION, WOUND", "WOUND_REPAIR", PD)
  PD <- gsub("ABDOMINAL", "", PD)
  PD <- gsub("DIRECT", "", PD)
  PD <- gsub("CLOSURE, WOUND", "WOUND_REPAIR", PD)
  PD <- gsub("LOWER EXTREMITY REPAIR", "", PD)
  PD <- gsub("HERNIA", "HERNIA_REPAIR", PD)
  PD <- gsub("UMBILICAL", "", PD)
  PD <- gsub("HIP", "", PD)
  PD <- gsub("EVACUATION, HEMATOMA", "HEMATOMA_EVACUATION", PD)
  PD <- gsub("CHEST OR ABDOMEN", "", PD)
  PD <- gsub("DIAGNOSTIC", "", PD)
  PD <- gsub("WITH GYNECOLOGIC PROCEDURE HYSTERECTOMY", "HYSTERECTOMY", PD)
  PD <- gsub("ROBOT-ASSISTED LYSIS, ADHESIONS", "ROBOT_ASSISTED_LYSIS_OF_ADHESIONS", PD)
  PD <- gsub("HEAD OR NECK", "", PD)
  PD <- gsub("HEAD OR NECK", "", PD)
  PD <- gsub("INTESTINAL, LAPAROSCOPIC", "", PD)
  PD <- gsub("ANEURYSM", "", PD)
  PD <- gsub("AORTA", "", PD)
  PD <- gsub("TRICUSPID VALVE", "", PD)
  PD <- gsub("WITH", "", PD)
  PD <- gsub("MITRAL VALVE", "", PD)
  PD <- gsub("APPROACH", "", PD)
  PD <- gsub("PERCUTANEOUS", "", PD)
  PD <- gsub("ENDOSCOPIC", "", PD)
  PD <- gsub("IRRIGATION AND DEBRIDEMENT", "IRRIGATION_AND_DEBRIDEMENT", PD, fixed=TRUE)
  PD <- gsub("IRRIGATION / DEBRIDEMENT CHEST / ABDOMEN", "IRRIGATION_AND_DEBRIDEMENT", PD, fixed=TRUE)
  PD <- gsub("FLEXIBLE FIBEROPTIC ESOPHAGOGASTRODUODENOSCOPY (EGD)", "EGD", PD, fixed=TRUE)
  PD <- gsub("ESOPHAGOGASTRODUODENOSCOPY (EGD)", "EGD", PD, fixed=TRUE)
  PD <- gsub("ESOPHAGOGASTRODUODENOSCOPY (EGD) GASTROSTOMY", "EGD", PD, fixed=TRUE)
  PD <- gsub("INTRAMEDULLARY RODDING, TIBIA", "INTRAMEDULLARY_RODDING_TIBIA", PD, fixed=TRUE)
  PD <- gsub("REPLACEMENT, AORTIC VALVE", "REPLACEMENT_AORTIC VALVE", PD, fixed=TRUE)
  PD <- gsub("ELBOW ORIF", "ELBOW, ORIF", PD, fixed=TRUE)
  PD <- gsub("FOOT OR ANKLE SEPTOPLASTY", "FOOT OR ANKLE, SEPTOPLASTY", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, PELVIS", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, FEMUR", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, MANDIBLE", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, ACETABULUM", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ANKLE ORIF", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ELBOW ORIF", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("HUMERUS ORIF", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("FEMUR ORIF", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("FOOT OR ANKLE LOWER_ORIF", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("LOWER EXTREMITY LOWER_ORIF", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("TIBIA AND FIBULA ORIF", "LOWER_ORIF", PD, fixed=TRUE)
  PD <- gsub("UPPER EXTREMITY ORIF", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, ELBOW", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ORIF, FRACTURE, HUMERUS", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("ANTERIOR  ORIF, FOREARM", "UPPER_ORIF", PD, fixed=TRUE)
  PD <- gsub("LOWER EXTREMITY ORIF", "LOWER_ORIF", PD, fixed=TRUE)


  #### finish claning and organize into a data.table ####
  proc <- NULL
  for(i in 1:length(PD)){

    spl <- strsplit(PD[i], split = ",")[[1]]

    dt <- data.table(spl, rep(DT$recno[i], length(spl)))
    if(nrow(dt) > 0){
      dt <- dt[!dt$spl == "",]
      dt <- dt[!dt$spl == " ",]
      dt <- dt[!dt$spl == "  ",]
      dt <- dt[!dt$spl == "   ",]
      dt <- dt[!dt$spl == "    ",]
      setnames(dt, c("spl", "V2"), c("orprocedures", "recno"))
    }else{
      dt <- data.table(NA, DT$recno[i])
      setnames(dt, c("V1", "V2"), c("orprocedures", "recno"))
    }
    proc <- rbind(proc, dt)
  }
  #### trim spaces from the ends of the characters ####
  proc$orprocedures <- trimws(proc$orprocedures, which =("both"))
  return(proc)
}









#' BPcleaner
#'
#' transforms blood pressure readings from a continuous to categorical variable.
#'
#' @param  DT a data table containing a column named "recno". If type = "prior", a column labeled "bpprior", if type = "post", a column labeled "bpafter"
#' @param  type: either prior of post
#' @return a data table containing one column labeled "orprocedures" and a second colum labeled "recno" containing the initial record number the diagnosis originated from.
#' @author Brendan Gongol
#' @export

BPcleaner <- function(DT, type){


  #### Prior blood pressure ####
  if(type == "prior"){
    BPprior <- DT$bpprior
    SPL <- strsplit(BPprior, split =  "/")
    systolic_prior <- NULL
    diastolic_prior <- NULL
    for(i in 1:length(SPL)){
      systolic_prior[i] <- SPL[[i]][1]
      diastolic_prior[i] <- SPL[[i]][2]
    }
    BP_prior <- as.data.table(cbind(systolic_prior, diastolic_prior))
    BP_prior[is.na(BP_prior$systolic_prior)] = 0
    BP_prior[is.na(BP_prior$diastolic_prior)] = 0
    BP_prior$systolic_prior <- as.numeric(BP_prior$systolic_prior)
    BP_prior$diastolic_prior <- as.numeric(BP_prior$diastolic_prior)
    BP_prior
    #### add annotation column ####
    systolicprior_annotate <- NULL
    for(i in 1:nrow(BP_prior)){
      if(BP_prior$systolic_prior[i] > 139){
        systolicprior_annotate[i] <- "systolic hypertension"
      }
      else if(BP_prior$systolic_prior[i] == 0){
        systolicprior_annotate[i] <- "nothing"
      }
      else if(BP_prior$systolic_prior[i] < 90){
        systolicprior_annotate[i] <- "systolic hypotension"
      }
      else{
        systolicprior_annotate[i] <- "systolic normotension"
      }
    }
    systolicprior_annotate

    diastolicprior_annotate <- NULL
    for(i in 1:nrow(BP_prior)){
      if(BP_prior$diastolic_prior[i] > 80){
        diastolicprior_annotate[i] <- "diastolic hypertension"
      }
      else if(BP_prior$diastolic_prior[i] == 0){
        diastolicprior_annotate[i] <- "nothing"
      }
      else if(BP_prior$diastolic_prior[i] < 60){
        diastolicprior_annotate[i] <- "diastolic hypotension"
      }
      else{
        diastolicprior_annotate[i] <- "diastolic normotension"
      }
    }
    diastolicprior_annotate
    BP_prior <- cbind(BP_prior, systolicprior_annotate, diastolicprior_annotate, DT$recno)
    setnames(BP_prior, c(colnames(BP_prior)), c(colnames(BP_prior)[1:ncol(BP_prior)-1], "recno"))

    return(BP_prior)
  }

  if(type == "post"){
    BPpost <- DT$`bpafter`
    SPL <- strsplit(BPpost, split =  "/")
    systolic_post <- NULL
    diastolic_post <- NULL
    for(i in 1:length(SPL)){
      systolic_post[i] <- SPL[[i]][1]
      diastolic_post[i] <- SPL[[i]][2]
    }
    BP_post <- as.data.table(cbind(systolic_post, diastolic_post))
    BP_post[is.na(BP_post)] = 0 ##### change to as.integer here.
    BP_post$systolic_post <- as.numeric(BP_post$systolic_post)
    BP_post$diastolic_post <- as.numeric(BP_post$diastolic_post)
    BP_post

    #### add annotation column ####
    systolicpost_annotate <- NULL
    for(i in 1:nrow(BP_post)){
      if(BP_post$systolic_post[i] > 139){
        systolicpost_annotate[i] <- "systolic hypertension"
      }
      else if(BP_post$systolic_post[i] == 0){
        systolicpost_annotate[i] <- "nothing"
      }
      else if(BP_post$systolic_post[i] < 90){
        systolicpost_annotate[i] <- "systolic hypotension"
      }
      else{
        systolicpost_annotate[i] <- "systolic normotension"
      }
    }
    systolicpost_annotate

    diastolicpost_annotate <- NULL
    for(i in 1:nrow(BP_post)){
      if(BP_post$diastolic_post[i] > 80){
        diastolicpost_annotate[i] <- "diastolic hypertension"
      }
      else if(BP_post$diastolic_post[i] == 0){
        diastolicpost_annotate[i] <- "nothing"
      }
      else if(BP_post$diastolic_post[i] < 60){
        diastolicpost_annotate[i] <- "diastolic hypotension"
      }
      else{
        diastolicpost_annotate[i] <- "diastolic normotension"
      }
    }
    diastolicpost_annotate

    BP_post <- cbind(BP_post, systolicpost_annotate, diastolicpost_annotate, DT$recno)
    setnames(BP_post, c(colnames(BP_post)), c(colnames(BP_post)[1:ncol(BP_post)-1], "recno"))

    return(BP_post)
  }

}








#' Labcleanr
#'
#' cleans the lab value annotation in a data table column.
#'
#' @param  DT a data table containing a column named "recno". If type = "prior", a column labeled "labsprior", if type = "post", a column labeled "labsafter"
#' @param  type: either prior of post
#' @return a data table containing one column labeled "labs_prior", a column labeled "values_prior", and a third colum labeled "recno" containing the initial record number the diagnosis originated from.
#' @author Brendan Gongol
#' @export

Labcleanr <- function(DT, type){

  if(type == "prior"){
    LP <- DT$labsprior

    LP <- gsub("IMMATURE GRANULOCYTES, %", "IMMATURE_GRANULOCYTES_% 0.5%", LP, fixed=TRUE)
    LP <- gsub("IMMATURE GRANULOCYTES, ABS", "IMMATURE_GRANULOCYTES_ABS", LP, fixed=TRUE)
    LP <- gsub("CA, IONIZED (BG)", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("CA, IONIZED", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("IONIZEDCA", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("CAIONIZED", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("IMMATURE RETIC FRACTION", "IMMATURE_RETIC_FRACTION", LP, fixed=TRUE)
    LP <- gsub("UREA N, UR RAND", "RANDOM_UREA_NITROGEN", LP, fixed=TRUE)
    LP <- gsub(", SQUAM EPI &lt;1 /hpf", "", LP, fixed=TRUE)
    LP <- gsub("GLU, RANDOM", "GLUCOSE, RANDOM (P) (QST)", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE &gt;=", "GLUCOSE_TOLERANCE ", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE, BF", "BODY_FLUID_GLUCOSE", LP, fixed=TRUE)
    LP <- gsub("CREAT, UR RAND", "RANDOM_URINE_CREATNINE", LP, fixed=TRUE)
    LP <- gsub("RBC (UA) &gt;", "RBC_UR ", LP, fixed=TRUE)
    LP <- gsub("VANCO, TROUGH", " VANCO_TROUGH", LP, fixed=TRUE)
    LP <- gsub("K, UR RAND", " K_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("NA, UR RAND", " NA_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("LYMPHS, ABS", " LYMPHS_ABS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D LYMPHS", "FLUID_C/D_LYMPHS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D NUC CELLS", "FLUID_C/D_NUC_CELLS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D RBC,", " FLUID_C/D_RBC", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D SEGS", "FLUID_C/D_SEGS", LP, fixed=TRUE)
    LP <- gsub(", FLUID DIFF DONE ON 100 Cells", "", LP, fixed=TRUE)
    LP <- gsub(" /cu mm", " /cumm", LP, fixed=TRUE)
    LP <- gsub("PROTEIN, BF", "PROTEIN_BF", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Rare /hpf", "BACTERIA Rare_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("HYALINE", "HYALINE_UA", LP, fixed=TRUE)
    LP <- gsub("MUCUS Present /lpf", "MUCUS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CL, UR RAND", "CL_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("EOSINOPHILS None Seen /hpf", "EOSINOPHILS_UA 0 hpf", LP, fixed=TRUE)
    LP <- gsub("EPITHELIAL CELLS Moderate /hpf", "EPITHELIAL_CELLS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("POLYS Few /hpf", "POLYS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("PROT, UR RAND", "PROT_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("mg/g Creat", "mg/g", LP, fixed=TRUE)
    LP <- gsub("WBC CLUMPS Present /hpf", "WBC_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub(", MPV Not measured fL", "", LP, fixed=TRUE)
    LP <- gsub("AMORPH SED Present /hpf", "AMORPH_SED_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("OSMO, UR RAND", "OSMO_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("YEAST Many /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("REN/URO EPI &lt;1 /hpf", "REN/URO_EPI_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("PO2 VEN", "PO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PCO2 VEN", "PCO2_VEN", LP, fixed=TRUE)
    LP <- gsub("YEAST Few /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CL, UR", "CL_UR", LP, fixed=TRUE)
    LP <- gsub("mMol/Coll Period", "mMol/Coll", LP, fixed=TRUE)
    LP <- gsub("CL, URINE", "CL_URINE", LP, fixed=TRUE)
    LP <- gsub("MYCELIAL Rare /hpf", "MYCELIAL_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("NA, UR", "NA_UR", LP, fixed=TRUE)
    LP <- gsub("CREAT, UR", "CREAT_UR", LP, fixed=TRUE)
    LP <- gsub("mg/Coll Period", "mg/Coll", LP, fixed=TRUE)
    LP <- gsub("MICROALBUMIN, UR", "MICROALBUMIN_UR", LP, fixed=TRUE)
    LP <- gsub("PROT, UR", "PROT_UR", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Few /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("ETHANOL LEVEL &lt;", "ETHANOL ", LP, fixed=TRUE)
    LP <- gsub("GENT, PEAK", "GENT_PEAK", LP, fixed=TRUE)
    LP <- gsub("FREE T4, EIA", "FREE_T4_EIA", LP, fixed=TRUE)
    LP <- gsub("HBSAB, QNT", "HBSAB_QNT", LP, fixed=TRUE)
    LP <- gsub("VITAMIN B12 &gt;", "VITAMIN_B12 ", LP, fixed=TRUE)
    LP <- gsub("NRBC, ABS", "NRBC_ABS", LP, fixed=TRUE)
    LP <- gsub("NRBC/100 WBC", "NRBC", LP, fixed=TRUE)
    LP <- gsub("BANDS, MANUAL", "BANDS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("LYMPHS, MANUAL", "LYMPHS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("MONOS, MANUAL", "MONOS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("SEGS, MANUAL", "SEGS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("BASOS, MANUAL", "BASOS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("YEAST Rare /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("MYCELIAL Few /hpf", "MYCELIAL_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TOTAL B-HCG, QUANT", "TOTAL B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("See comment", "NA", LP, fixed=TRUE)
    LP <- gsub("EOSINS, MANUAL", "EOSIN_MANUAL", LP, fixed=TRUE)
    LP <- gsub("?1.29", "1.29", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Moderate /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CA OXALATE Moderate /hpf", "CA_OXALATE_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Many /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("K, UR", "K_UR", LP, fixed=TRUE)
    LP <- gsub("C-REACTIVE PROTEIN", "C-REACTIVE_PROTEIN", LP, fixed=TRUE)
    LP <- gsub("CA OXALATE Rare /hpf", "CA_OXALATE_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("DIR BILI", "DIR_BILI", LP, fixed=TRUE)
    LP <- gsub("HDL CHOL", "HDL_CHOL", LP, fixed=TRUE)
    LP <- gsub("LDL CHOL", "LDL_CHOL", LP, fixed=TRUE)
    LP <- gsub("cm H2O", "cmH2O", LP, fixed=TRUE)
    LP <- gsub("VOLUME, UR", "VOLUME_UR", LP, fixed=TRUE)
    LP <- gsub("URIC ACID Rare /hpf", "URIC_ACID_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TRIP PHOS Rare /hpf", "TRIP_PHOS 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TROPONIN T", "TROPONIN_T", LP, fixed=TRUE)
    LP <- gsub("OSMO, URINE", "OSMO_URINE", LP, fixed=TRUE)
    LP <- gsub("URIC ACID Few /hpf", "URIC_ACID_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("EPITHELIAL CELLS Few /hpf", "EPITHELIAL_CELLS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("POLYS Many /hpf", "POLYS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("NEURON SPECIFIC ENOLASE, SERUM", "NEURON_SPECIFIC_ENOLASE_SERUM", LP, fixed=TRUE)
    LP <- gsub("YEAST Moderate /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("METHYLMALONIC ACID, QN", "METHYLMALONIC ACID_QN", LP, fixed=TRUE)
    LP <- gsub(", FREE BY DIALYSIS", "", LP, fixed=TRUE)
    LP <- gsub("AMYLASE, BF", "AMYLASE_BF", LP, fixed=TRUE)
    LP <- gsub("BE ART", "BE_ART", LP, fixed=TRUE)
    LP <- gsub("IN ERROR", "NA", LP, fixed=TRUE)
    LP <- gsub("VANCO LEVEL", "VANCO_LEVEL", LP, fixed=TRUE)
    LP <- gsub("ALBUMIN, BF", "ALBUMIN_BF", LP, fixed=TRUE)
    LP <- gsub("CREAT, BF", "CREAT_BF", LP, fixed=TRUE)
    LP <- gsub(", MPV SEE COMMENT. fL", "", LP, fixed=TRUE)
    LP <- gsub(", PLTS SEE COMMENT. bil/L", "", LP, fixed=TRUE)
    LP <- gsub("OSMOLALITY, STOOL", "OSMOLALITY_STOOL", LP, fixed=TRUE)
    LP <- gsub("BUN", "BUN_LABCORP", LP, fixed=TRUE)
    LP <- gsub("RANDOM P", "PHOS", LP, fixed=TRUE)
    LP <- gsub("NA ", "NA_SERUM ", LP, fixed=TRUE)
    LP <- gsub("NA_SERUM (BG)", "NA_SERUM", LP, fixed=TRUE)
    LP <- gsub("B-HCG_QUANT", "TOTAL_B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("TOTAL B-HCG, QUANT", "TOTAL_B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("CBC ", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("CBC_OTHEROTHER", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("CBC_OTHER35", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("GRAN CASTS", "GRAN_CASTS", LP, fixed=TRUE)
    LP <- gsub("PLASMA CELLS", "PLASMA_CELLS", LP, fixed=TRUE)
    LP <- gsub("VARIANT LYMPHS", "VARIANT_LYMPHS", LP, fixed=TRUE)
    LP <- gsub("HGB (POC)", "HGB_POC", LP, fixed=TRUE)
    LP <- gsub("MIXED CELL", "MIXED_CELL", LP, fixed=TRUE)
    LP <- gsub("UIBC (FE)", "UIBC_FE", LP, fixed=TRUE)
    LP <- gsub("TIBC (FE)", "TIBC_FE", LP, fixed=TRUE)
    LP <- gsub("NA_UR (MMOL/COLL PERIOD)", "NA_UR_MMOL/COLL_PERIOD", LP, fixed=TRUE)
    LP <- gsub("VAR LYMPH", "VAR_LYMPH", LP, fixed=TRUE)
    LP <- gsub("URIC A", "URIC_A", LP, fixed=TRUE)
    LP <- gsub("QTC INTERVAL", "QTC_INTERVAL", LP, fixed=TRUE)
    LP <- gsub("QT INTERVAL", "QT_INTERVAL", LP, fixed=TRUE)
    LP <- gsub("RETIC ", "RETICULOCYTE_COUNT ", LP, fixed=TRUE)
    LP <- gsub("RETICULOCYTE ", "RETICULOCYTE_HGB", LP, fixed=TRUE)
    LP <- gsub("RETICULOCYTE_HGBHGB ", "RETICULOCYTE_HGB ", LP, fixed=TRUE)
    LP <- gsub("SQUAM EPI", "SQUAM_EPI", LP, fixed=TRUE)
    LP <- gsub("REN/URO EPI", "REN/URO_EPI_UA", LP, fixed=TRUE)
    LP <- gsub("PROT_UR_RAND (MG/G CREAT)", "PROTUR_MG/G_CREAT", LP, fixed=TRUE)
    LP <- gsub("PROT_UR (MG/G CREAT)", "PROTUR_MG/G_CREAT", LP, fixed=TRUE)
    LP <- gsub("BE ART", "BE_ART", LP, fixed=TRUE)
    LP <- gsub("BE VEN", "BE_VEN", LP, fixed=TRUE)
    LP <- gsub("BE CAP", "BE_CAP", LP, fixed=TRUE)
    LP <- gsub("VALPROIC ACID", "VALPROIC_ACID", LP, fixed=TRUE)
    LP <- gsub("RBC (UA)", "RBC_UA", LP, fixed=TRUE)
    LP <- gsub("FLUID DIFF DONE ON", "FLUID_DIFF_DONE_ON", LP, fixed=TRUE)
    LP <- gsub("FLUID_C/D_RBC.", "FLUID_C/D_RBC", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D OTHER", "FLUID_C/D_OTHER", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D BANDS", "FLUID_C/D_BANDS", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE (POC)", "GLUCOSE_POC", LP, fixed=TRUE)
    LP <- gsub("FLUID_C/D_NUC_CELLS.", "FLUID_C/D_NUC_CELLS", LP, fixed=TRUE)
    LP <- gsub("ANTITHROMBIN III ACTIVITY", "ANTITHROMBIN_III_ACTIVITY", LP, fixed=TRUE)
    LP <- gsub("ANTITHROMBIN III ANTIGEN", "ANTITHROMBIN_III_ANTIGEN", LP, fixed=TRUE)
    LP <- gsub("NEUT %", "NEUT_%", LP, fixed=TRUE)
    LP <- gsub("MONO #", "MONO_#", LP, fixed=TRUE)
    LP <- gsub("MONO %", "MONO_%", LP, fixed=TRUE)
    LP <- gsub("EOSIN #", "EOSIN_#", LP, fixed=TRUE)
    LP <- gsub("EOSIN %", "EOSIN_%", LP, fixed=TRUE)
    LP <- gsub("BASO #", "BASO_#", LP, fixed=TRUE)
    LP <- gsub("BASO %", "BASO_%", LP, fixed=TRUE)
    LP <- gsub("SPONT RESP RATE (BG)", "SPONT_RESP_RATE", LP, fixed=TRUE)
    LP <- gsub("VT (SPONT) (BG)", "VT_SPONT", LP, fixed=TRUE)
    LP <- gsub("TIME HIGH (BG)", "TIME_HIGH", LP, fixed=TRUE)
    LP <- gsub("TIME LOW (BG)", "TIME_LOW", LP, fixed=TRUE)
    LP <- gsub("PRESSURE HIGH (BG)", "PRESSURE_HIGH", LP, fixed=TRUE)
    LP <- gsub("PRESSURE LOW (BG)", "PRESSURE_LOW", LP, fixed=TRUE)
    LP <- gsub("NITRIC OXIDE (BG)", "NITRIC_OXIDE", LP, fixed=TRUE)
    LP <- gsub("EXPIR TIME (BG)", "EXPIR_TIME", LP, fixed=TRUE)
    LP <- gsub("FLOW RATE (BG)", "FLOW_RATE", LP, fixed=TRUE)
    LP <- gsub("WBC (UA)", "WBC_UA", LP, fixed=TRUE)
    LP <- gsub("BACK-UP RATE (BG)", "BACK-UP_RATE", LP, fixed=TRUE)
    LP <- gsub("MECH RESP RATE (BG)", "MECH_RESP_RATE", LP, fixed=TRUE)
    LP <- gsub("INSPIR TIME (BG)", "INSPIR_TIME", LP, fixed=TRUE)
    LP <- gsub("TOT PROT ", "PROT_T ", LP, fixed=TRUE)
    LP <- gsub("TOT BILI", "BILI_T ", LP, fixed=TRUE)
    LP <- gsub("VT (MECH) (BG)", "VT_MECH", LP, fixed=TRUE)
    LP <- gsub("SO2 ART", "SO2_ART", LP, fixed=TRUE)
    LP <- gsub("SO2 VEN", "SO2_VEN", LP, fixed=TRUE)
    LP <- gsub("SO2 CAP", "SO2_CAP", LP, fixed=TRUE)
    LP <- gsub("PO2 ART", "PO2_ART", LP, fixed=TRUE)
    LP <- gsub("PO2 VEN", "PO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PO2 CAP", "PO2_CAP", LP, fixed=TRUE)
    LP <- gsub("PEAK AIRWAY PRESSURE (BG)", "PEAK_AIRWAY_PRESSURE", LP, fixed=TRUE)
    LP <- gsub("PCO2 ART", "PCO2_ART", LP, fixed=TRUE)
    LP <- gsub("PCO2 VEN", "PCO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PCO2 CAP", "PCO2_CAP", LP, fixed=TRUE)
    LP <- gsub("O2 CONTENT (A)", "O2_CONTENT_A", LP, fixed=TRUE)
    LP <- gsub("O2 CAPACITY (A)", "O2_CAPACITY_A", LP, fixed=TRUE)
    LP <- gsub("MINUTE VENT (BG)", "MINUTE_VENT", LP, fixed=TRUE)
    LP <- gsub("HCO3 ART", "HCO3_ART", LP, fixed=TRUE)
    LP <- gsub("HCO3 VEN", "HCO3_VEN", LP, fixed=TRUE)
    LP <- gsub("HCO3 CAP", "HCO3_CAP", LP, fixed=TRUE)
    LP <- gsub("FO2HGB (A)", "FO2HGB_A", LP, fixed=TRUE)
    LP <- gsub("FMETHGB (A)", "FMETHGB_A", LP, fixed=TRUE)
    LP <- gsub("FCOHGB (A)", "FCOHGB_A", LP, fixed=TRUE)
    LP <- gsub("GLU, RANDOM", "GLU_RANDOM", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE, RANDOM (P) (QST)", "GLU_RANDOM", LP, fixed=TRUE)
    LP <- gsub("CL_UR (MMOL/COLL PERIOD)", "CL_UR_MMOL/COLL_PERIOD", LP, fixed=TRUE)
    LP <- gsub("HEPARIN-PF4 AB (HIT) REACTIVITY", "HEPARIN-PF4_AB_HIT_REACTIVITY", LP, fixed=TRUE)
    LP <- gsub("ALDOSTERONE &lt;", "ALDOSTERONE ", LP, fixed=TRUE)
    LP <- gsub("LMW HEPARIN", "LMW_HEPARIN", LP, fixed=TRUE)
    LP <- gsub("T4 (THYROXINE)", "T4_FREE_DIALYSIS", LP, fixed=TRUE)
    LP <- gsub("METHYLMALONIC ACID_QN", "METHYLMALONIC_ACID", LP, fixed=TRUE)
    LP <- gsub("TEG MA (MAXIMUM AMPLITUDE)", "TEG_MA_MAXIMUM_AMPLITUDE", LP, fixed=TRUE)
    LP <- gsub("TEG LY30 (30 MIN % LYSIS)", "TEG_LY30", LP, fixed=TRUE)
    LP <- gsub("TEG K TIME", "TEG_K_TIME", LP, fixed=TRUE)
    LP <- gsub("TEG G (CLOT STRENGTH)", "TEG_G_CLOT_STRENGTH", LP, fixed=TRUE)
    LP <- gsub("TEG ANGLE (ALPHA)", "TEG_ANGLE_ALPHA", LP, fixed=TRUE)
    LP <- gsub("TEG R", "TEG_R", LP, fixed=TRUE)
    LP <- gsub("INTACT PTH", "INTACT_PTH", LP, fixed=TRUE)
    LP <- gsub("SERVO PRESSURE (BG)", "SERVO_PRESSURE", LP, fixed=TRUE)
    LP <- gsub("RHEUMATOID FACTOR", "RHEUMATOID_FACTOR", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D MYELO", "FLUID_C/D_MYELO", LP, fixed=TRUE)
    LP <- gsub("UREA N, UR", "UREA_N_UR", LP, fixed=TRUE)
    LP <- gsub("IMMATURE PLATELET FRACTION (IPF)", "IMMATURE_PLATELET_FRACTION_IPF", LP, fixed=TRUE)
    LP <- gsub("VOLUME_UR (ML)", "VOLUME_UR_ML", LP, fixed=TRUE)
    LP <- gsub("C3 COMPLEMENT", "C3_COMPLEMENT", LP, fixed=TRUE)
    LP <- gsub("C4 COMPLEMENT", "C4_COMPLEMENT", LP, fixed=TRUE)
    LP <- gsub("VITAMIN", "VITAMIN_B12", LP, fixed=TRUE)
    LP <- gsub("PLASMA HEMOGLOBIN", "PLASMA_HEMOGLOBIN", LP, fixed=TRUE)
    LP <- gsub("&lt;", "", LP, fixed=TRUE)
    LP <- gsub("ACT PLUS (POC)", "ACT", LP, fixed=TRUE)



    # z <- 1
    # i<- 45
    #### Identify all labs and remove duplicates ####################
    lab_tot <- NULL
    for(z in 1:length(LP)){

      p <- LP[z] # 15, 17, 20, 21,
      if(!p==""){
        p2 <- strsplit(p, split = ",")
        p3 <- strsplit(p2[[1]], split = ",", perl = TRUE)

        labs <- NULL
        values <- NULL
        for(i in 1:length(p3)){
          p4 <- strsplit(p3[[i]], split = ")")
          p4[[1]];length(p4[[1]])

          if(length(p4[[1]]) == 1){

            if(grepl("&", p4[[1]][1]) == TRUE){
              t <- trimws(p4)
              t <- strsplit(t, split = " ", perl = TRUE)

              labs[i] <- t[[1]][1]
              labs

              t2 <- strsplit(t[[1]][2], split = ";")
              values[i] <- t2[[1]][length(t2[[1]])]
              values
            }else if(grepl("TOT", p4[[1]][1]) == TRUE){
              p5 <- strsplit(p4[[1]][1], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][2]
              labs

              values[i] <- p5[[1]][[length(p5[[1]])-1]]
              values
            }else if(grepl("UREA", p4[[1]][1]) == TRUE){
              # p5 <- strsplit(p4[[1]][1], split = " ", perl = TRUE)
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][1]
              labs
              if(p5[[1]][2] == "N"){
                values[i] <- 1
              }else{
                values[i] <- 2
              }
              values
            }else{
              p5 <- strsplit(p4[[1]][1], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][1]
              labs
              if(length(p5[[1]]) > 1){
                values[i] <- p5[[1]][[length(p5[[1]])-1]]
                values
              }else{
                values[i] <- NA
              }
            }

          }else if(length(p4[[1]]) == 2){

            if(grepl("&", p4[[1]][2]) == TRUE){
              q <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1])
              labs[i] <- trimws(gsub("BG", "", q))
              labs

              t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
              t <- t[[1]][length(t[[1]]) - 1]
              t <- strsplit(t, split = ";")
              values[i] <- t[[1]][2]
              values

            }else {
              q <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1])
              labs[i] <- trimws(gsub("BG", "", q))
              labs

              t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
              values[i] <- t[[1]][length(t[[1]]) - 1]
              values
            }
          }else if(length(p4[[1]]) == 3){
            labs[i] <-  trimws(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1]))


            t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
            values[i] <- t[[1]][length(t[[1]]) - 1]
            values
          }
        }
        dat <- as.data.frame(cbind(labs, values))

        lab_tot[[z]] <- dat
      }
    }


    #### transform each labs prior into a data table an rbind them together ####
    labpriorDT <- NULL
    for(i in 1:length(lab_tot)){
      if(length(lab_tot[[i]][1]) > 0){
        dt <- data.table(lab_tot[[i]][1], lab_tot[[i]][2], rep(DT$recno[i], nrow(lab_tot[[i]][1])))
        setnames(dt, c("labs", "values", "V3"), c("labs_prior", "values_prior","recno"))
        labpriorDT <- rbind(labpriorDT, dt)
      }else{
        dt <- data.table(cbind("N/A","N/A", DT$recno[i]))
        setnames(dt, c("V1", "V2", "V3"), c("labs_prior", "values_prior","recno"))
        labpriorDT <- rbind(labpriorDT, dt)
      }

    }
    labpriorDT$recno <- as.numeric(labpriorDT$recno)
    labpriorDT[labpriorDT$values_prior == "N/A",]$values_prior <- NA

    return(labpriorDT)
  }

  if(type == "post"){
    LP <- DT$labsafter

    LP <- gsub("IMMATURE GRANULOCYTES, %", "IMMATURE_GRANULOCYTES_% 0.5%", LP, fixed=TRUE)
    LP <- gsub("IMMATURE GRANULOCYTES, ABS", "IMMATURE_GRANULOCYTES_ABS", LP, fixed=TRUE)
    LP <- gsub("CA, IONIZED (BG)", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("CA, IONIZED", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("IONIZEDCA", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("CAIONIZED", "CA_IONIZED", LP, fixed=TRUE)
    LP <- gsub("IMMATURE RETIC FRACTION", "IMMATURE_RETIC_FRACTION", LP, fixed=TRUE)
    LP <- gsub("UREA N, UR RAND", "RANDOM_UREA_NITROGEN", LP, fixed=TRUE)
    LP <- gsub(", SQUAM EPI &lt;1 /hpf", "", LP, fixed=TRUE)
    LP <- gsub("GLU, RANDOM", "GLUCOSE, RANDOM (P) (QST)", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE &gt;=", "GLUCOSE_TOLERANCE ", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE, BF", "BODY_FLUID_GLUCOSE", LP, fixed=TRUE)
    LP <- gsub("CREAT, UR RAND", "RANDOM_URINE_CREATNINE", LP, fixed=TRUE)
    LP <- gsub("RBC (UA) &gt;", "RBC_UR ", LP, fixed=TRUE)
    LP <- gsub("VANCO, TROUGH", " VANCO_TROUGH", LP, fixed=TRUE)
    LP <- gsub("K, UR RAND", " K_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("NA, UR RAND", " NA_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("LYMPHS, ABS", " LYMPHS_ABS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D LYMPHS", "FLUID_C/D_LYMPHS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D NUC CELLS", "FLUID_C/D_NUC_CELLS", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D RBC,", " FLUID_C/D_RBC", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D SEGS", "FLUID_C/D_SEGS", LP, fixed=TRUE)
    LP <- gsub(", FLUID DIFF DONE ON 100 Cells", "", LP, fixed=TRUE)
    LP <- gsub(" /cu mm", " /cumm", LP, fixed=TRUE)
    LP <- gsub("PROTEIN, BF", "PROTEIN_BF", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Rare /hpf", "BACTERIA Rare_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("HYALINE", "HYALINE_UA", LP, fixed=TRUE)
    LP <- gsub("MUCUS Present /lpf", "MUCUS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CL, UR RAND", "CL_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("EOSINOPHILS None Seen /hpf", "EOSINOPHILS_UA 0 hpf", LP, fixed=TRUE)
    LP <- gsub("EPITHELIAL CELLS Moderate /hpf", "EPITHELIAL_CELLS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("POLYS Few /hpf", "POLYS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("PROT, UR RAND", "PROT_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("mg/g Creat", "mg/g", LP, fixed=TRUE)
    LP <- gsub("WBC CLUMPS Present /hpf", "WBC_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub(", MPV Not measured fL", "", LP, fixed=TRUE)
    LP <- gsub("AMORPH SED Present /hpf", "AMORPH_SED_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("OSMO, UR RAND", "OSMO_UR_RAND", LP, fixed=TRUE)
    LP <- gsub("YEAST Many /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("REN/URO EPI &lt;1 /hpf", "REN/URO_EPI_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("PO2 VEN", "PO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PCO2 VEN", "PCO2_VEN", LP, fixed=TRUE)
    LP <- gsub("YEAST Few /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CL, UR", "CL_UR", LP, fixed=TRUE)
    LP <- gsub("mMol/Coll Period", "mMol/Coll", LP, fixed=TRUE)
    LP <- gsub("CL, URINE", "CL_URINE", LP, fixed=TRUE)
    LP <- gsub("MYCELIAL Rare /hpf", "MYCELIAL_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("NA, UR", "NA_UR", LP, fixed=TRUE)
    LP <- gsub("CREAT, UR", "CREAT_UR", LP, fixed=TRUE)
    LP <- gsub("mg/Coll Period", "mg/Coll", LP, fixed=TRUE)
    LP <- gsub("MICROALBUMIN, UR", "MICROALBUMIN_UR", LP, fixed=TRUE)
    LP <- gsub("PROT, UR", "PROT_UR", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Few /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("ETHANOL LEVEL &lt;", "ETHANOL ", LP, fixed=TRUE)
    LP <- gsub("GENT, PEAK", "GENT_PEAK", LP, fixed=TRUE)
    LP <- gsub("FREE T4, EIA", "FREE_T4_EIA", LP, fixed=TRUE)
    LP <- gsub("HBSAB, QNT", "HBSAB_QNT", LP, fixed=TRUE)
    LP <- gsub("VITAMIN B12 &gt;", "VITAMIN_B12 ", LP, fixed=TRUE)
    LP <- gsub("NRBC, ABS", "NRBC_ABS", LP, fixed=TRUE)
    LP <- gsub("NRBC/100 WBC", "NRBC", LP, fixed=TRUE)
    LP <- gsub("BANDS, MANUAL", "BANDS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("LYMPHS, MANUAL", "LYMPHS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("MONOS, MANUAL", "MONOS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("SEGS, MANUAL", "SEGS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("BASOS, MANUAL", "BASOS_MANUAL", LP, fixed=TRUE)
    LP <- gsub("YEAST Rare /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("MYCELIAL Few /hpf", "MYCELIAL_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TOTAL B-HCG, QUANT", "TOTAL B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("See comment", "NA", LP, fixed=TRUE)
    LP <- gsub("EOSINS, MANUAL", "EOSIN_MANUAL", LP, fixed=TRUE)
    LP <- gsub("?1.29", "1.29", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Moderate /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("CA OXALATE Moderate /hpf", "CA_OXALATE_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("BACTERIA Many /hpf", "BACTERIA_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("K, UR", "K_UR", LP, fixed=TRUE)
    LP <- gsub("C-REACTIVE PROTEIN", "C-REACTIVE_PROTEIN", LP, fixed=TRUE)
    LP <- gsub("CA OXALATE Rare /hpf", "CA_OXALATE_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("DIR BILI", "DIR_BILI", LP, fixed=TRUE)
    LP <- gsub("HDL CHOL", "HDL_CHOL", LP, fixed=TRUE)
    LP <- gsub("LDL CHOL", "LDL_CHOL", LP, fixed=TRUE)
    LP <- gsub("cm H2O", "cmH2O", LP, fixed=TRUE)
    LP <- gsub("VOLUME, UR", "VOLUME_UR", LP, fixed=TRUE)
    LP <- gsub("URIC ACID Rare /hpf", "URIC_ACID_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TRIP PHOS Rare /hpf", "TRIP_PHOS 1 hpf", LP, fixed=TRUE)
    LP <- gsub("TROPONIN T", "TROPONIN_T", LP, fixed=TRUE)
    LP <- gsub("OSMO, URINE", "OSMO_URINE", LP, fixed=TRUE)
    LP <- gsub("URIC ACID Few /hpf", "URIC_ACID_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("EPITHELIAL CELLS Few /hpf", "EPITHELIAL_CELLS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("POLYS Many /hpf", "POLYS_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("NEURON SPECIFIC ENOLASE, SERUM", "NEURON_SPECIFIC_ENOLASE_SERUM", LP, fixed=TRUE)
    LP <- gsub("YEAST Moderate /hpf", "YEAST_UA 1 hpf", LP, fixed=TRUE)
    LP <- gsub("METHYLMALONIC ACID, QN", "METHYLMALONIC ACID_QN", LP, fixed=TRUE)
    LP <- gsub(", FREE BY DIALYSIS", "", LP, fixed=TRUE)
    LP <- gsub("AMYLASE, BF", "AMYLASE_BF", LP, fixed=TRUE)
    LP <- gsub("BE ART", "BE_ART", LP, fixed=TRUE)
    LP <- gsub("IN ERROR", "NA", LP, fixed=TRUE)
    LP <- gsub("VANCO LEVEL", "VANCO_LEVEL", LP, fixed=TRUE)
    LP <- gsub("ALBUMIN, BF", "ALBUMIN_BF", LP, fixed=TRUE)
    LP <- gsub("CREAT, BF", "CREAT_BF", LP, fixed=TRUE)
    LP <- gsub(", MPV SEE COMMENT. fL", "", LP, fixed=TRUE)
    LP <- gsub(", PLTS SEE COMMENT. bil/L", "", LP, fixed=TRUE)
    LP <- gsub("OSMOLALITY, STOOL", "OSMOLALITY_STOOL", LP, fixed=TRUE)
    LP <- gsub("BUN", "BUN_LABCORP", LP, fixed=TRUE)
    LP <- gsub("RANDOM P", "PHOS", LP, fixed=TRUE)
    LP <- gsub("NA ", "NA_SERUM ", LP, fixed=TRUE)
    LP <- gsub("NA_SERUM (BG)", "NA_SERUM", LP, fixed=TRUE)
    LP <- gsub("B-HCG_QUANT", "TOTAL_B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("TOTAL B-HCG, QUANT", "TOTAL_B-HCG_QUANT", LP, fixed=TRUE)
    LP <- gsub("CBC ", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("CBC_OTHEROTHER", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("CBC_OTHER35", "CBC_OTHER", LP, fixed=TRUE)
    LP <- gsub("GRAN CASTS", "GRAN_CASTS", LP, fixed=TRUE)
    LP <- gsub("PLASMA CELLS", "PLASMA_CELLS", LP, fixed=TRUE)
    LP <- gsub("VARIANT LYMPHS", "VARIANT_LYMPHS", LP, fixed=TRUE)
    LP <- gsub("HGB (POC)", "HGB_POC", LP, fixed=TRUE)
    LP <- gsub("MIXED CELL", "MIXED_CELL", LP, fixed=TRUE)
    LP <- gsub("UIBC (FE)", "UIBC_FE", LP, fixed=TRUE)
    LP <- gsub("TIBC (FE)", "TIBC_FE", LP, fixed=TRUE)
    LP <- gsub("NA_UR (MMOL/COLL PERIOD)", "NA_UR_MMOL/COLL_PERIOD", LP, fixed=TRUE)
    LP <- gsub("VAR LYMPH", "VAR_LYMPH", LP, fixed=TRUE)
    LP <- gsub("URIC A", "URIC_A", LP, fixed=TRUE)
    LP <- gsub("QTC INTERVAL", "QTC_INTERVAL", LP, fixed=TRUE)
    LP <- gsub("QT INTERVAL", "QT_INTERVAL", LP, fixed=TRUE)
    LP <- gsub("RETIC ", "RETICULOCYTE_COUNT ", LP, fixed=TRUE)
    LP <- gsub("RETICULOCYTE ", "RETICULOCYTE_HGB", LP, fixed=TRUE)
    LP <- gsub("RETICULOCYTE_HGBHGB ", "RETICULOCYTE_HGB ", LP, fixed=TRUE)
    LP <- gsub("SQUAM EPI", "SQUAM_EPI", LP, fixed=TRUE)
    LP <- gsub("REN/URO EPI", "REN/URO_EPI_UA", LP, fixed=TRUE)
    LP <- gsub("PROT_UR_RAND (MG/G CREAT)", "PROTUR_MG/G_CREAT", LP, fixed=TRUE)
    LP <- gsub("PROT_UR (MG/G CREAT)", "PROTUR_MG/G_CREAT", LP, fixed=TRUE)
    LP <- gsub("BE ART", "BE_ART", LP, fixed=TRUE)
    LP <- gsub("BE VEN", "BE_VEN", LP, fixed=TRUE)
    LP <- gsub("BE CAP", "BE_CAP", LP, fixed=TRUE)
    LP <- gsub("VALPROIC ACID", "VALPROIC_ACID", LP, fixed=TRUE)
    LP <- gsub("RBC (UA)", "RBC_UA", LP, fixed=TRUE)
    LP <- gsub("FLUID DIFF DONE ON", "FLUID_DIFF_DONE_ON", LP, fixed=TRUE)
    LP <- gsub("FLUID_C/D_RBC.", "FLUID_C/D_RBC", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D OTHER", "FLUID_C/D_OTHER", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D BANDS", "FLUID_C/D_BANDS", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE (POC)", "GLUCOSE_POC", LP, fixed=TRUE)
    LP <- gsub("FLUID_C/D_NUC_CELLS.", "FLUID_C/D_NUC_CELLS", LP, fixed=TRUE)
    LP <- gsub("ANTITHROMBIN III ACTIVITY", "ANTITHROMBIN_III_ACTIVITY", LP, fixed=TRUE)
    LP <- gsub("ANTITHROMBIN III ANTIGEN", "ANTITHROMBIN_III_ANTIGEN", LP, fixed=TRUE)
    LP <- gsub("NEUT %", "NEUT_%", LP, fixed=TRUE)
    LP <- gsub("MONO #", "MONO_#", LP, fixed=TRUE)
    LP <- gsub("MONO %", "MONO_%", LP, fixed=TRUE)
    LP <- gsub("EOSIN #", "EOSIN_#", LP, fixed=TRUE)
    LP <- gsub("EOSIN %", "EOSIN_%", LP, fixed=TRUE)
    LP <- gsub("BASO #", "BASO_#", LP, fixed=TRUE)
    LP <- gsub("BASO %", "BASO_%", LP, fixed=TRUE)
    LP <- gsub("SPONT RESP RATE (BG)", "SPONT_RESP_RATE", LP, fixed=TRUE)
    LP <- gsub("VT (SPONT) (BG)", "VT_SPONT", LP, fixed=TRUE)
    LP <- gsub("TIME HIGH (BG)", "TIME_HIGH", LP, fixed=TRUE)
    LP <- gsub("TIME LOW (BG)", "TIME_LOW", LP, fixed=TRUE)
    LP <- gsub("PRESSURE HIGH (BG)", "PRESSURE_HIGH", LP, fixed=TRUE)
    LP <- gsub("PRESSURE LOW (BG)", "PRESSURE_LOW", LP, fixed=TRUE)
    LP <- gsub("NITRIC OXIDE (BG)", "NITRIC_OXIDE", LP, fixed=TRUE)
    LP <- gsub("EXPIR TIME (BG)", "EXPIR_TIME", LP, fixed=TRUE)
    LP <- gsub("FLOW RATE (BG)", "FLOW_RATE", LP, fixed=TRUE)
    LP <- gsub("WBC (UA)", "WBC_UA", LP, fixed=TRUE)
    LP <- gsub("BACK-UP RATE (BG)", "BACK-UP_RATE", LP, fixed=TRUE)
    LP <- gsub("MECH RESP RATE (BG)", "MECH_RESP_RATE", LP, fixed=TRUE)
    LP <- gsub("INSPIR TIME (BG)", "INSPIR_TIME", LP, fixed=TRUE)
    LP <- gsub("TOT PROT ", "PROT_T ", LP, fixed=TRUE)
    LP <- gsub("TOT BILI", "BILI_T ", LP, fixed=TRUE)
    LP <- gsub("VT (MECH) (BG)", "VT_MECH", LP, fixed=TRUE)
    LP <- gsub("SO2 ART", "SO2_ART", LP, fixed=TRUE)
    LP <- gsub("SO2 VEN", "SO2_VEN", LP, fixed=TRUE)
    LP <- gsub("SO2 CAP", "SO2_CAP", LP, fixed=TRUE)
    LP <- gsub("PO2 ART", "PO2_ART", LP, fixed=TRUE)
    LP <- gsub("PO2 VEN", "PO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PO2 CAP", "PO2_CAP", LP, fixed=TRUE)
    LP <- gsub("PEAK AIRWAY PRESSURE (BG)", "PEAK_AIRWAY_PRESSURE", LP, fixed=TRUE)
    LP <- gsub("PCO2 ART", "PCO2_ART", LP, fixed=TRUE)
    LP <- gsub("PCO2 VEN", "PCO2_VEN", LP, fixed=TRUE)
    LP <- gsub("PCO2 CAP", "PCO2_CAP", LP, fixed=TRUE)
    LP <- gsub("O2 CONTENT (A)", "O2_CONTENT_A", LP, fixed=TRUE)
    LP <- gsub("O2 CAPACITY (A)", "O2_CAPACITY_A", LP, fixed=TRUE)
    LP <- gsub("MINUTE VENT (BG)", "MINUTE_VENT", LP, fixed=TRUE)
    LP <- gsub("HCO3 ART", "HCO3_ART", LP, fixed=TRUE)
    LP <- gsub("HCO3 VEN", "HCO3_VEN", LP, fixed=TRUE)
    LP <- gsub("HCO3 CAP", "HCO3_CAP", LP, fixed=TRUE)
    LP <- gsub("FO2HGB (A)", "FO2HGB_A", LP, fixed=TRUE)
    LP <- gsub("FMETHGB (A)", "FMETHGB_A", LP, fixed=TRUE)
    LP <- gsub("FCOHGB (A)", "FCOHGB_A", LP, fixed=TRUE)
    LP <- gsub("GLU, RANDOM", "GLU_RANDOM", LP, fixed=TRUE)
    LP <- gsub("GLUCOSE, RANDOM (P) (QST)", "GLU_RANDOM", LP, fixed=TRUE)
    LP <- gsub("CL_UR (MMOL/COLL PERIOD)", "CL_UR_MMOL/COLL_PERIOD", LP, fixed=TRUE)
    LP <- gsub("HEPARIN-PF4 AB (HIT) REACTIVITY", "HEPARIN-PF4_AB_HIT_REACTIVITY", LP, fixed=TRUE)
    LP <- gsub("ALDOSTERONE &lt;", "ALDOSTERONE ", LP, fixed=TRUE)
    LP <- gsub("LMW HEPARIN", "LMW_HEPARIN", LP, fixed=TRUE)
    LP <- gsub("T4 (THYROXINE)", "T4_FREE_DIALYSIS", LP, fixed=TRUE)
    LP <- gsub("METHYLMALONIC ACID_QN", "METHYLMALONIC_ACID", LP, fixed=TRUE)
    LP <- gsub("TEG MA (MAXIMUM AMPLITUDE)", "TEG_MA_MAXIMUM_AMPLITUDE", LP, fixed=TRUE)
    LP <- gsub("TEG LY30 (30 MIN % LYSIS)", "TEG_LY30", LP, fixed=TRUE)
    LP <- gsub("TEG K TIME", "TEG_K_TIME", LP, fixed=TRUE)
    LP <- gsub("TEG G (CLOT STRENGTH)", "TEG_G_CLOT_STRENGTH", LP, fixed=TRUE)
    LP <- gsub("TEG ANGLE (ALPHA)", "TEG_ANGLE_ALPHA", LP, fixed=TRUE)
    LP <- gsub("TEG R", "TEG_R", LP, fixed=TRUE)
    LP <- gsub("INTACT PTH", "INTACT_PTH", LP, fixed=TRUE)
    LP <- gsub("SERVO PRESSURE (BG)", "SERVO_PRESSURE", LP, fixed=TRUE)
    LP <- gsub("RHEUMATOID FACTOR", "RHEUMATOID_FACTOR", LP, fixed=TRUE)
    LP <- gsub("FLUID C/D MYELO", "FLUID_C/D_MYELO", LP, fixed=TRUE)
    LP <- gsub("UREA N, UR", "UREA_N_UR", LP, fixed=TRUE)
    LP <- gsub("IMMATURE PLATELET FRACTION (IPF)", "IMMATURE_PLATELET_FRACTION_IPF", LP, fixed=TRUE)
    LP <- gsub("VOLUME_UR (ML)", "VOLUME_UR_ML", LP, fixed=TRUE)
    LP <- gsub("C3 COMPLEMENT", "C3_COMPLEMENT", LP, fixed=TRUE)
    LP <- gsub("C4 COMPLEMENT", "C4_COMPLEMENT", LP, fixed=TRUE)
    LP <- gsub("VITAMIN", "VITAMIN_B12", LP, fixed=TRUE)
    LP <- gsub("PLASMA HEMOGLOBIN", "PLASMA_HEMOGLOBIN", LP, fixed=TRUE)
    LP <- gsub("&lt;", "", LP, fixed=TRUE)
    LP <- gsub("ACT PLUS (POC)", "ACT", LP, fixed=TRUE)

    #### Identify all labs and remove duplicates ####################
    lab_tot <- NULL
    for(z in 1:length(LP)){

      p <- LP[z] # 15, 17, 20, 21,
      if(!p==""){
        p2 <- strsplit(p, split = ",")
        p3 <- strsplit(p2[[1]], split = ",", perl = TRUE)

        labs <- NULL
        values <- NULL
        for(i in 1:length(p3)){
          p4 <- strsplit(p3[[i]], split = ")")
          p4[[1]];length(p4[[1]])

          if(length(p4[[1]]) == 1){

            if(grepl("&", p4[[1]][1]) == TRUE){
              t <- trimws(p4)
              t <- strsplit(t, split = " ", perl = TRUE)

              labs[i] <- t[[1]][1]
              labs

              t2 <- strsplit(t[[1]][2], split = ";")
              values[i] <- t2[[1]][length(t2[[1]])]
              values
            }else if(grepl("TOT", p4[[1]][1]) == TRUE){
              p5 <- strsplit(p4[[1]][1], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][2]
              labs

              values[i] <- p5[[1]][[length(p5[[1]])-1]]
              values
            }else if(grepl("UREA", p4[[1]][1]) == TRUE){
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][1]
              labs
              if(p5[[1]][2] == "N"){
                values[i] <- 1
              }else{
                values[i] <- 2
              }
              values
            }else{
              p5 <- strsplit(p4[[1]][1], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
              p5 <- trimws(p4)
              p5 <- strsplit(p5, split = " ")
              labs[i] <- p5[[1]][1]
              labs
              if(length(p5[[1]]) > 1){
                values[i] <- p5[[1]][[length(p5[[1]])-1]]
                values
              }else{
                values[i] <- NA
              }
            }

          }else if(length(p4[[1]]) == 2){

            if(grepl("&", p4[[1]][2]) == TRUE){
              q <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1])
              labs[i] <- trimws(gsub("BG", "", q))
              labs

              t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
              t <- t[[1]][length(t[[1]]) - 1]
              t <- strsplit(t, split = ";")
              values[i] <- t[[1]][2]
              values

            }else {
              q <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1])
              labs[i] <- trimws(gsub("BG", "", q))
              labs

              t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
              values[i] <- t[[1]][length(t[[1]]) - 1]
              values
            }
          }else if(length(p4[[1]]) == 3){
            labs[i] <-  trimws(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", p4[[1]][1]))
            # labs

            t <- strsplit(p4[[1]][length(p4[[1]])], split = " ", perl = TRUE)
            values[i] <- t[[1]][length(t[[1]]) - 1]
            values
          }
        }
        dat <- as.data.frame(cbind(labs, values))

        lab_tot[[z]] <- dat
      }
    }

    #### transform each labs prior into a data table an rbind them together ####
    labpriorDT <- NULL
    for(i in 1:length(lab_tot)){
      if(length(lab_tot[[i]][1]) > 0){
        dt <- data.table(lab_tot[[i]][1], lab_tot[[i]][2], rep(DT$recno[i], nrow(lab_tot[[i]][1])))
        setnames(dt, c("labs", "values", "V3"), c("labs_after", "values_after","recno"))
        labpriorDT <- rbind(labpriorDT, dt)
      }else{
        dt <- data.table(cbind("N/A","N/A", DT$recno[i]))
        setnames(dt, c("V1", "V2", "V3"), c("labs_after", "values_after","recno"))
        labpriorDT <- rbind(labpriorDT, dt)
      }

    }
    labpriorDT$recno <- as.numeric(labpriorDT$recno)
    labpriorDT[labpriorDT$values_after == "N/A",]$values_after <- NA

    return(labpriorDT)
  }
}








#' Labcleanr
#'
#' transforms lab values from a continuous to categorical variable.
#'
#' @param  DT a data table containing a column named "low" and a column named "high". If type = "prior", a column labeled "labs_prior" and a column labeled "values_prior", if type = "post", a column labeled "labs_after" and a column labeled "values_after".
#' @param  type: either prior of post
#' @return a vector with annotated lab values
#' @author Brendan Gongol
#' @export

labAnnotatR <- function(DT, type){

  if(type == "prior"){
    DT <- DT[!is.na(DT$low),]
    DT <- DT[!is.na(DT$high),]
    lab_annotation <- NULL
    for(i in 1:nrow(DT)){
      if(DT$labs_prior[i] == "N/A"){
        lab_annotation[i] <- paste(DT$labs_prior[i], "NONE", sep = "_")
      }else{

        if(DT$values_prior[i] < DT$low[i]){
          lab_annotation[i] <- paste(DT$labs_prior[i], "LOW", sep = "_")
        }else if(DT$values_prior[i] > DT$high[i]){
          lab_annotation[i] <- paste(DT$labs_prior[i], "HIGH", sep = "_")
        }else{
          lab_annotation[i] <- paste(DT$labs_prior[i], "NORMAL", sep = "_")
        }

      }
    }
    return(lab_annotation)
  }


  if(type == "post"){
    DT <- DT[!is.na(DT$low),]
    DT <- DT[!is.na(DT$high),]
    lab_annotation <- NULL
    for(i in 1:nrow(DT)){
      if(DT$labs_after[i] == "N/A"){
        lab_annotation[i] <- paste(DT$labs_after[i], "NONE", sep = "_")
      }else{

        if(DT$values_after[i] < DT$low[i]){
          lab_annotation[i] <- paste(DT$labs_after[i], "LOW", sep = "_")
        }else if(DT$values_after[i] > DT$high[i]){
          lab_annotation[i] <- paste(DT$labs_after[i], "HIGH", sep = "_")
        }else{
          lab_annotation[i] <- paste(DT$labs_after[i], "NORMAL", sep = "_")
        }

      }
    }
    return(lab_annotation)
  }


}




#' MedsCleanR
#'
#' cleans the medications column of a data table.
#'
#' @param  DT a data table containing a column named "recno". If type = "prior", a column labeled "medsprior", if type = "after", a column labeled "medsafter"
#' @param  type: either prior of post
#' @return a data table containing one column labeled "labs_prior", a column labeled "values_prior", and a third colum labeled "recno" containing the initial record number the diagnosis originated from.
#' @author Brendan Gongol
#' @export

MedsCleanR <- function(DT, type){

  if(type == "prior"){
    PD <- DT$medsprior
    PD <- gsub("1,000", "1000", PD)
    PD <- gsub("1,250", "1250", PD)
    PD <- gsub("2,400", "2400", PD)
    PD <- gsub("5,000", "5000", PD)
    PD <- gsub("10,000", "10000", PD)
    PD <- gsub("ALBUMIN, HUMAN", "ALBUMIN", PD)
    PD

    dtmed <- NULL
    for(a in 1:length(PD)){
      PD2 <- strsplit(PD[a], split = ",")

      med <- NULL
      if(length(strsplit(PD[a], split = ",")[[1]]) > 0){
        for(i in 1:length(PD2[[1]])){
          med[i] <- gsub("[0-9].*", "", PD2[[1]][i]) #### remove everything after first number
        }
        med <- trimws(med, which =("both")) #### trim spaces off ends
        dt <- data.table(cbind(med, DT$recno[a]))
        setnames(dt, c("med", "V2"), c("meds_prior", "recno"))
        dtmed <- rbind(dtmed, dt)

      }else{

        dt <- data.table(cbind("N/A", DT$recno[a]))
        setnames(dt, c("V1", "V2"), c("meds_prior", "recno"))
        dtmed <- rbind(dtmed, dt)
      }

    }
    dtmed$recno <- as.numeric(dtmed$recno)
    return(dtmed)
  }


  if(type == "after"){
    PD <- DTsub$medsafter
    PD <- gsub("1,000", "1000", PD)
    PD <- gsub("1,250", "1250", PD)
    PD <- gsub("2,400", "2400", PD)
    PD <- gsub("5,000", "5000", PD)
    PD <- gsub("10,000", "10000", PD)
    PD <- gsub("ALBUMIN, HUMAN", "ALBUMIN", PD)
    PD

    dtmed <- NULL
    for(a in 1:length(PD)){
      PD2 <- strsplit(PD[a], split = ",")

      med <- NULL
      if(length(strsplit(PD[a], split = ",")[[1]]) > 0){
        for(i in 1:length(PD2[[1]])){
          med[i] <- gsub("[0-9].*", "", PD2[[1]][i]) #### remove everything after first number
        }
        med <- trimws(med, which =("both")) #### trim spaces off ends
        dt <- data.table(cbind(med, DTsub$recno[a]))
        setnames(dt, c("med", "V2"), c("meds_after", "recno"))
        dtmed <- rbind(dtmed, dt)

      }else{

        dt <- data.table(cbind("N/A", DTsub$recno[a]))
        setnames(dt, c("V1", "V2"), c("meds_after", "recno"))
        dtmed <- rbind(dtmed, dt)
      }

    }
    dtmed$recno <- as.numeric(dtmed$recno)
    return(dtmed)
  }

}






#' PUscoreR
#'
#' scores pressure ulcer severity.
#'
#' @param  DT a data table containing labels specified according to the diagnosis, procedure, lab, or meds cleaning functions.
#' @param  diag either TRUE or FALSE
#' @param  proc either TRUE or FALSE
#' @param  lab either TRUE or FALSE
#' @param  med either TRUE or FALSE
#' @return a vector with scored values
#' @author Brendan Gongol
#' @export

PUscoreR <- function(DT, diag, proc, lab, med){

  pb <- txtProgressBar(min = 0, max = nrow(DT), style = 3)
  SCO <- NULL
  SU <- NULL
  for(i in 1:nrow(DT)){
    sub <- DT[i,]

    if(diag == TRUE){
      if(sub$patientdiagnosis > 0){
        diagsub <- DiagnosisCleaner(DT = sub)
        diagsub$recno <- as.numeric(diagsub$recno)
        diagsub
      }}

    if(proc == TRUE){
      if(sub$orprocedures > 0){
        if(!sub$orprocedures == "NULL"){
          procsub <- ProcedureCleaner(DT = sub)
          procsub
        }}}

    if(lab == TRUE){
      if(sub$labsprior > 0){
        if(!sub$labsprior == "NULL"){
          labpriorsub <- Labcleanr(DT =sub, type = "prior")
          labpriorsub$labs_prior <- as.character(labpriorsub$labs_prior)
          labpriorsub$values_prior <- as.numeric(labpriorsub$values_prior)
          #### Annotate with high/low values ####
          normval <- fread("./normal lab values/Normal lab values.csv")
          setnames(normval, colnames(normval), c("labs_prior", "low", "high")   )
          LJsub <- merge(x = labpriorsub, y = normval, by = "labs_prior", all.x = TRUE)
          LJsub <- LJsub[order(LJsub$recno),]
          LJsub <- LJsub[!is.na(LJsub$low),]
          LJsub <- LJsub[!is.na(LJsub$high),]
          LJsub <- LJsub[!is.na(LJsub$values_prior),]
          lab_annotation <- labAnnotatR(LJsub, type = "prior")
          LJsub$labannotation <- lab_annotation
          LJsub <- data.table(LJsub)
          LJsub
        }}}

    if(med == TRUE){
      if(sub$medsprior > 0){
        medsub <- MedsCleanR(DT = sub, type = "prior")
        medsub
      }}


    if(exists("diagsub")){
      DS <- merge(diagsub, diagscore, by = "Annotation", all.x = TRUE) # merge by "diagnosis" if want to use the diagnosis code instead of the annotation.
      DS <- DS[!is.na(DS$score)]
      SU[1] <- sum(DS$score)
    }
    if(exists("procsub")){
      PS <- merge(procsub, procscore, by = "orprocedures", all.x = TRUE)
      PS <- PS[!is.na(PS$score)]
      SU[2] <- sum(PS$score)
    }
    if(exists("LJsub")){
      LS <- merge(LJsub, labscore, by = "labannotation", all.x = TRUE)
      LS <- LS[!is.na(LS$score)]
      SU[3] <- sum(LS$score)
    }
    if(exists("medsub")){
      MS <- merge(medsub, medscore, by = "meds_prior", all.x = TRUE)
      MS <- MS[!is.na(MS$score)]
      SU[4] <- sum(MS$score)
    }
    SU <- SU[!is.na(SU)]
    SU <- sum(SU)

    SUDT <- data.table(SU, DT$recno[i], DT$type[i])
    SCO<- rbind(SCO, SUDT)
    SU <- NULL

    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(SCO)
}























