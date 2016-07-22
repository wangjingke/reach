setwd(output)

library(XLConnect)
# location of columns for mothers and children's IDs
sp16=readWorksheetFromFile(log, sheet=1, header=TRUE, startCol=min(dyad.col), endCol=max(dyad.col))
id.list=sp16[c("Mother.PID", "Child.PID")]
id.list$No=1:nrow(id.list)

{
    repeat {
        print(id.list)
        indiv = readline(
            "Please choose what you want to do (input the number of the selection) \n
            1. download EMA surveys for all these participants \n
            2. select dyads, and download their EMA surveys \n
            3. input IDs of individuals and download their EMA surveys"
        )
        if (indiv == "3") {
            keep = c()
            repeat {
                include = readline("Please input the ID of the individuals you want to download, seperated by a comma")
                keep = c(keep, include)
                done = readline("Are you done with the input? (yes/no)")
                if (grepl("y", done)) {break}
            }
            input.list = toupper(gsub(" ", "", strsplit(keep, ",")[[1]]))
            print(input.list)
            continue = readline("These are the people you choose to download, should we continue (yes/no)?")
            if (grepl("y", continue)) {folder.list = unlist(short.list[c(1,2)], use.names = FALSE); break}
        } else if (indiv == "2") {
            keep = c()
            repeat {
                include = readline("Please input the No. of the dyads you want to download, seperated by a comma")
                keep = c(keep, include)
                done = readline("Are you done with the input? (yes/no)")
                if (grepl("y", done)) {break}
            }
            short.list = id.list[as.numeric(strsplit(keep, ",")[[1]]),]
            print(short.list)
            continue = readline("These are the people you choose to download, should we continue (yes/no)?")
            if (grepl("y", continue)) {folder.list = unlist(short.list[c(1,2)], use.names = FALSE); break}
        } else {
            if (indiv == "1") {folder.list = unlist(id.list[c(1,2)], use.names = FALSE); break}
        }
    }

    # make folder to store compliance data in local machine
    local_md = paste0("! md compliance_",Sys.Date())
    sftp.command = local_md
    for (i in 1:length(folder.list)) {
        # make folder
        mdX = paste0("! md compliance_", Sys.Date(),"\\MATCH_", folder.list[i], "\\.match\\survey")
        # copy files
        copyX = paste0(
            "get -r /data/MATCH_", folder.list[i],"/.match/survey compliance_",Sys.Date(),"/MATCH_",folder.list[i], "/.match/survey"
        )
        sftp.command = c(sftp.command, mdX, copyX)
    }

    # output script
    script_output=readline("Do you want to output the script (yes/no)?")
    if (grepl("y", script_output)) {
        write.table(data.frame(sftp.command), paste0(script.name,".txt"), row.names = FALSE, col.names = FALSE, quote=FALSE, sep="\t")
        print(paste0("psfty script is created, and stored under ", output))
        } else {print("No script is created")}

    # excute script
    # command=paste0("psftp -b ", script.name,".txt -be viewer_match@wockets.ccs.neu.edu -pw ######")
    # shell(command, intern=FALSE)
}
