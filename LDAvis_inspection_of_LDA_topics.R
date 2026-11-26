#  Uses 200-topic LDA model and makes its topic distributions and word associations easier to inspect interactively. 
#  Intended as a supplementary tool when manually reviewing and labelling the 200 topics.
#  The next function takes 2 arguments and specifically:
# - the 'topicModel' object (from the 'Untitled.RData' object)
# - the 'DTM' object (from the 'Untitled.RData' object)

# Required R packages
if(!require(pacman)) install.packages("pacman");
pacman::p_load(LDAvis,
               magrittr, 
               topicmodels,
               Matrix,
               servr,
               install = TRUE)

# the path to the 'Untitled.RData' object that includes the 'topicModel', 'DTM' objects
pth = "/Users/anonymous/Desktop/Username_textanalysis/Untitled.RData"
load(pth)
ls()

# Directory to save the output files of the function (the LDAViz creates 5 files)
DIR_OUT = "/Users/anonymous/Desktop/Username_textanalysis/DIR_VIZ"

# create the sparse matrix fromt the DTM (required as input to the 'topicmodels_json_ldavis' function)
doc_term_obj = Matrix::sparseMatrix(i=DTM$i, 
                                    j=DTM$j,
                                    x=DTM$v, 
                                    dims=c(DTM$nrow, DTM$ncol), 
                                    dimnames = DTM$dimnames)


# visualization using ldavis
topicmodels_json_ldavis = function(fitted,
                                   doc_term) {
  
  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)

  # temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(doc_term),
                            Freq = as.vector(colSums(doc_term)))
  # rm(temp_frequency)
  
  doc_length = as.vector(rowSums(doc_term))
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, 
                                 theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  return(json_lda)
}


# use the 'topicModel', 'DTM' as input to the 'topicmodels_json_ldavis' function
viz_obj = topicmodels_json_ldavis(fitted = topicModel,
                                   doc_term = doc_term_obj) 

# using open.browser = TRUE, the function will open a server, we can then navigate to for 
# instance http://127.0.0.1:4321 (this applies to my PC and will be different every time and in a different operating system)
serVis(viz_obj, open.browser = TRUE, out.dir = DIR_OUT)


# stop the server once I viewed and played with the interactive visualization in the web browser
servr::daemon_stop(1)

