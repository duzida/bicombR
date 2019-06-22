#abstract profiles class
setClass("ABprofile", representation(PMID = "character", TI = "character", AB = "character", 
                                    TA = "character", PDAT = "character", ISSN = "character", 
                                    MH = "list", SH = "list", MAJR = "list", AD = "list", AU = "list"))
