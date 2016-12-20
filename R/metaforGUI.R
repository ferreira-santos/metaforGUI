#' metaforGUI: Graphical User Interface (GUI) for the R metafor Package
#' @description
#' Cross-platform GUI for the R metafor[1] package, based on gWidgets and RGtk2. Allows conducting a basic meta-analysis. Suitable for beginners not familiar with R, but does not allow using advanced features of metafor.
#' @seealso
#' \code{\link[metafor]{metafor-package}} for full details of the 'metafor' package.
#' @examples
#' # Simply run the main function to bring up the GUI.
#' metaforGUI()
#' # Loading data set, selecting variables and running analysis are run from the GUI.
#' @references
#' [1] Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import metafor
#' @export
metaforGUI <- function(){

#Cool Guide: http://user2007.org/program/presentations/verzani-1.pdf
# https://rdrr.io/rforge/gWidgets/

# change icon: http://stackoverflow.com/questions/14764517/how-do-i-change-the-wm-icon-for-a-gwindow-using-gwidgets-in-r

# First run:   install.packages("gWidgetsRGtk2", dep=TRUE)

# You can learn more about package authoring with RStudio at: http://r-pkgs.had.co.nz/
#https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf

#require(gWidgetsRGtk2)
#require(metafor)
options(guiToolkit = "RGtk2")

#Get Screen size (pixels)
sw <- as.numeric(system("wmic desktopmonitor get screenwidth" , intern=TRUE))[-c(1, length(as.numeric(system("wmic desktopmonitor get screenwidth" , intern=TRUE))))][2]
sh <- as.numeric(system("wmic desktopmonitor get screenheight", intern=TRUE))[-c(1, length(as.numeric(system("wmic desktopmonitor get screenheight", intern=TRUE))))][2]
  #print(c("Screen Size:",as.character(sw), as.character(sh)))
start_width <- sw/2
start_height <- sh/2
rm(sh, sw)

win = gwindow("VisualMetafor (FFS)", width=start_width, height=start_height)
rm(start_width, start_height)
#lay = glayout(container=win) # lay = layout organizer
g <- gframe("Define:", horizontal=F, container=win, expand=T)
g_top <- ggroup(container=g)
  glabel("  DataSet file:", container=g_top)
  editFile <- gedit(text="<no file>", container=g_top)
  gbutton("Load file", container=g_top, handler=
  function(h,...) {
  # get locale
  # load file according to locale (read.csv or read.csv2) using --> gfile()
  })

  gseparator(container=g)

g_mid <- ggroup(container=g, expand=T)
  g_mid_left <- ggroup(container=g_mid)
    varbrowser <- gvarbrowser(container=g_mid_left)
  g_mid_right <- ggroup(container=g_mid, horizontal=F)
    addSpring(g_mid_right)

    frameStudies <- gframe("Study names (optional):", container=g_mid_right, expand=T)
      glabel(" ", container=frameStudies)
      btnAddStudies <- gbutton(">>", container=frameStudies, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddStudies)==">>") {
            svalue(labelStudies) <- svalue(varbrowser)
            svalue(btnAddStudies) <- "<<"}
          else {
            svalue(labelStudies) <- "No variable selected"
            svalue(btnAddStudies) <- ">>"}
        })
      labelStudies <- glabel("No variable selected", container=frameStudies)
      glabel("  ", container=frameStudies)
    addSpring(g_mid_right)

    frameES <- gframe("Effect Sizes (ES):", container=g_mid_right, expand=T)
      glabel(" ", container=frameES)
      btnAddES <- gbutton(">>", container=frameES, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddES)==">>") {
            svalue(labelES) <- svalue(varbrowser)
            svalue(btnAddES) <- "<<"}
          else {
            svalue(labelES) <- "No variable selected"
            svalue(btnAddES) <- ">>"}
        })
      labelES <- glabel("No variable selected", container=frameES)
      glabel("  ", container=frameES)
    addSpring(g_mid_right)

    frameVar <- gframe("ES Variances:", container=g_mid_right, expand=T)
      glabel(" ", container=frameVar)
      btnAddVar <- gbutton(">>", container=frameVar, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddVar)==">>") {
            svalue(labelVar) <- svalue(varbrowser)
            svalue(btnAddVar) <- "<<"}
          else {
            svalue(labelVar) <- "No variable selected"
            svalue(btnAddVar) <- ">>"}
        })
      labelVar <- glabel("No variable selected", container=frameVar)
      glabel("  ", container=frameVar)
    addSpring(g_mid_right)

    frameN <- gframe("Sample Size (N):", container=g_mid_right, expand=T)
      glabel(" ", container=frameN)
      btnAddN <- gbutton(">>", container=frameN, handler=
        function(h,...) {
          if(svalue(varbrowser)=="") { gmessage("No variable selected from the variable list. Please make a selection first.\n\nIf there are no variables available you probably need to load your data set file.", title="Select variable first") }
          else if(svalue(btnAddN)==">>") {
            svalue(labelN) <- svalue(varbrowser)
            svalue(btnAddN) <- "<<"}
          else {
            svalue(labelN) <- "No variable selected"
            svalue(btnAddN) <- ">>"}
        })
      labelN <- glabel("No variable selected", container=frameN)
      glabel("  ", container=frameN)
    addSpring(g_mid_right)

  gseparator(container=g)

g_bottom <- ggroup(container=g)
  glabel("  Output dir (WD):", container=g_bottom)
  editWD <- gedit(getwd(), container=g_bottom)
  gbutton("Change dir", container=g_bottom, handler=
    function(h,...) {
      setwd(gfile(type="selectdir"))
      svalue(editWD) <- getwd()
    })
g2 <- ggroup(horizontal=F, container=win, expand=T)
g2frame <- gframe("Define and Run:", container=g2, horizontal=F, expand=T)
gbutton("Options", container=g2frame) #serves as header and clicking brings back defaults!
gradio(items=c("Fixed Effects","Random Effects"), selected=2, container=g2frame)
gbutton("Outputs", container=g2frame) #serves as header and clicking brings back defaults!
gcheckbox("Meta-Analysis Results", checked=T, container=g2frame)
gcheckbox("Plot Forest Plot", checked=T, container=g2frame)
gcheckbox("Plot Funnel Plot", checked=T, container=g2frame)
gcheckbox("Save Output as RData file", checked=T, container=g2frame)
  #  save(x, file="MA_output.RData")

addSpring(g2frame)
gbutton("Run Meta-Analysis", container=g2frame)
  #check if svalue(btnAddStudy)=="<<" (or maybe create global vars for the main args to pass to rma?)
  #check for all 4 vars
  #if no outputs are selected give warning saying no output files will be written
  #if files already exist, confirmation dialog asking if user wants to overwrite

}
#metaforGUI()


