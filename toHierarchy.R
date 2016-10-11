
library(data.tree)

# A function that takes a 2-column data.frame and prepares it for hierarchical form
toHierarchy <- function(d) {
    
    # hier_map looks for the parent in the child column until the right-most column contains no parent. Note that the data.frame must be prepared in a format containing 2 columns called "child" and "parent" from left to right.
    hier_map <<- function(m) { 
        i <- 2
        
        while( !all(is.na(m[,ncol(m)])) ){
            
            m <- data.frame(m, parentName = NA)  # initialize a new col
            colnames(m)[ncol(m)] <- paste0("parent", i)  # rename the col
            
            m[,ncol(m)] <- ifelse(
                m[,ncol(m)-1] %in% m$child, 
                m[match(m[,ncol(m)-1], m$child), 2], 
                NA
            )
            
            
            i = i + 1
        }
        m[,ncol(m)] <- NULL  # get rid of the last empty column
        return(m)
    }
    
    # Check classes of the input
    if( is.data.frame(d) ) {  # data.frame?
        if( all(sapply(d, is.character)) ) {  # all character vectors?
            # Insert data wrangling code here
            # rename the parent column
            colnames(df) <- gsub("parent", "parent1", colnames(df))
            
            df_hierarchy <- hier_map(d)  # apply the mapping function
            
            # cols <- grep("parent", colnames(df_hierarchy))  # get the columns to paste
#             cols <- 1:ncol(df_hierarchy)
#             cols <- rev(cols)    
        }
    }
    
    # Identify the "top-seed" parent
    # bigParent <- unique(df_hierarchy[!is.na(df_hierarchy[,ncol(df_hierarchy)]), ncol(df_hierarchy)])
    
    # Paste the columns together
    df_hierarchy$pathString <- do.call("paste", df_hierarchy[,ncol(df_hierarchy):1])
    
    # Paste the "org" as the top-seed
    df_hierarchy$pathString <- paste("org", df_hierarchy$pathString)
    
    # Paste the "top-seed" parent in the path string
    # df_hierarchy$pathString <- paste(bigParent, df_hierarchy$pathString)
    
    # Insert a "/" where the spaces are
    df_hierarchy$pathString <- gsub(" ", "/", df_hierarchy$pathString)
    
    # remove NAs from the path string  
    df_hierarchy$pathString <- gsub("/NA", "", df_hierarchy$pathString)
    
    # return the final dataset
    return(df_hierarchy)  
}
    

    
    org_chart <- as.Node(org)  # convert to nodes
    