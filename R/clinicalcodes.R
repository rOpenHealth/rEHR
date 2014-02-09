#' Downloads clinical codes file from ClinicalCodes.org
#' Either specify the full path or the article id and codelist name
#' If an article_id is supplied but no codelist_name, all codelists are downloaded and saved as a list
#' @param url character representing the url of a codelist download on www.clinicalcodes.org
#' @param article_id integer representiong the id of a www.ClinicalCodes.org article
#' @param codelist_name character representing a codelist name associated with an article on www.ClinicalCodes.org
#' @export
#' @return a dataframe of clinical codes or a list of dataframes of clinical codes
#' @examples \dontrun{
#' # Get codelist from url:
#' angina_codes <- get_ClinicalCodes(url = "https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/6/codelist/angina/download/")
#' depression_codes <- get_ClinicalCodes(article_id = 6, codelist_name = "depression")
#' codelists = get_ClinicalCodes(article_id = 6)
#' } 
get_ClinicalCodes <- function(url = NULL, article_id = NULL, codelist_name = NULL){
    if(!is.null(url)){
        clinicalcodes_regex <- "(www\\.[cC]{1}linical[cC]{1}odes.org)|(https://clinicalcodes.rss.mhs.man.ac.uk)/medcodes/article/(.)+/codelist/(.)+/download(/)*"
        if(!str_detect(url, clinicalcodes_regex)) stop("You must provide a valid www.ClinicalCodes.org codelist download link")
        url <- getURL(url)
        read.csv(text = url)
    } else if(!is.null(article_id) && !is.null(codelist_name)){
        url <- getURL(sprintf("https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/%d/codelist/%s/download/",
                              article_id, codelist_name))
        read.csv(text = url)
    } else if(!is.null(article_id)){
        article_url <- getURL(sprintf("https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/%d/",
                                      article_id))
        article_tree <- htmlTreeParse(article_url, error=function(...){}, useInternalNodes = TRUE)
        links <- unlist(xpathSApply(article_tree, "//*/a", xmlGetAttr, name = "href"))
        links <- links[str_detect(links, "download")]
        codelists <- lapply(links, function(link){
            url <- getURL(paste0("https://clinicalcodes.rss.mhs.man.ac.uk", link))
            read.csv(text=url)
        })
        names(codelists) <- basename(dirname(links))
        codelists
    } else stop("You must supply a url, an article id or an article id and a codelist name")
}


