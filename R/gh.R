#' Add Issue to Github Project
#'
#' @param gq GraphQL connection object from `get_gq()`
#' @param project_id Project ID
#' @param issue_id Issue ID
#' @param verbose Print GraphQL query; default=FALSE
#'
#' @return ID of new item in project
#' @importFrom ghql Query
#' @export
#'
#' @examples
#' \dontrun{
#'  token <- Sys.getenv("GITHUB_TOKEN")
#'  gq <- get_gq(token)
#'  prj <- get_project_id(gq, owner = "MarineSensitivities", num = 1)
#'  #TODO: get issues from repository
#'}
add_project_issue <- function(gq, project_id, issue_id, verbose = FALSE){
  # add issue to project, return id of new item in project

  qry <- ghql::Query$new()
  q <- glueb(
    'mutation {
        addProjectV2ItemById(
          input: {
            projectId: "[project_id]"
            contentId: "[issue_id]" } )
        {item {id}} }')
  if (verbose) message(q)
  qry$query('q', q )
  gq$exec(qry$queries$q) |>
    fromJSON() |>
    (\(x){ x$data$addProjectV2ItemById$item$id })()
}

#' Get GraphQL connection
#'
#'  Get GraphQL connection to Github. Refresh schema before returning.
#'
#' @param token Github personal access token. See \href{https://docs.github.com/en/graphql/guides/forming-calls-with-graphql#authenticating-with-graphql}{Authenticating with GraphQL}.
#' @param url GraphQL API URL. Default is \code{https://api.github.com/graphql}
#'
#' @return \link[ghql]{GraphqlClient} object
#' @importFrom ghql GraphqlClient
#' @export
#'
#' @examples
#' \dontrun{
#'   token <- Sys.getenv("GITHUB_TOKEN")
#'   gq <- get_gq(token)
#'   gq
#' }
get_gq <- function(token, url = "https://api.github.com/graphql"){

  con <- ghql::GraphqlClient$new(
    url     = "https://api.github.com/graphql",
    headers = list(Authorization = paste("Bearer", token)))
  con$load_schema()

  con
}

#' Get repositories for a Github user or organization
#'
#' @param owner Github user or organization name
#'
#' @return Character vector of repository names
#' @importFrom gh gh
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#'   get_owner_repos("MarineSensitivities")
#' }
get_owner_repos <- function(owner = "MarineSensitivities"){
  gh::gh(glue::glue("GET /orgs/{owner}/repos")) |>
    vapply("[[", "", "name") |>
    sort()
}

#' Get Github Project ID
#'
#' Needed to operate on GraphQL.
#'
#' @param gq GraphQL connection object from `get_gq()`
#' @param owner Github user or organization name containing the Project
#' @param project_num Project number
#'
#' @return Project ID
#' @importFrom ghql Query
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#'   token <- Sys.getenv("GITHUB_TOKEN")
#'   gq <- get_gq(token)
#'   get_project_id(gq, owner = "MarineSensitivities", num = 1)
#' }
get_project_id <- function(gq, owner = "MarineSensitivities", project_num = 1){
  # get Project ID, eg from
  #   https://github.com/orgs/MarineSensitivities/projects/1

  qry <- ghql::Query$new()
  q <- glueb(
    '{organization(login: "[owner]")
      { projectV2(number: [project_num])
        {id} } }')
  qry$query('q',  q)
  gq$exec(qry$queries$q) |>
    jsonlite::fromJSON() |>
    (\(x){ x$data$organization$projectV2$id })()
}

#' Get Project Issues
#'
#' @param gq GraphQL connection object from `get_gq()`
#' @param project_id Project ID from `get_project_id()`
#'
#' @return Character vector of issue node IDs, with titles as names
#' @importFrom ghql Query
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#'  token <- Sys.getenv("GITHUB_TOKEN")
#'  gq <- get_gq(token)
#'  prj <- get_project_id(gq, owner = "MarineSensitivities", num = 1)
#'  get_project_issues(gq, prj)
#' }
get_project_issues <- function(gq, project_id){
  # return IDs of issues in project

  qry <- Query$new()
  qry$query(
    'q', glueb(
      '{ node(id: "[project_id]") {
      ... on ProjectV2 {
        items(first: 20) {
          nodes{
            id
            content{
              ...on Issue {
                id title }
      }}}}}}') )
  j <- gq$exec(qry$queries$q) |>
    jsonlite::fromJSON()
  ids <- j |> (\(x){ x$data$node$items$nodes$content$id })()
  titles <- j |> (\(x){ x$data$node$items$nodes$content$title })()
  setNames(ids, titles)
}

#' Get Issues from Github repository
#'
#' @param owner Github user or organization name containing issues
#' @param repo Github repository name
#'
#' @return Character vector of issue node IDs
#' @importFrom gh gh
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#'  get_repo_issues("MarineSensitivities", "server")
#' }
get_repo_issues <- function(owner = "MarineSensitivities", repo = "server"){
  gh::gh(glue::glue("GET /repos/{owner}/{repo}/issues")) |>
    vapply("[[", "", "node_id")
}


#' Glue with brackets
#'
#' Since GraphQL statements heavily use curly braces `{}`,
#' this function is a wrapper around \link[glue]{glue} to make it easier to
#' write GraphQL statements.
#'
#' @param ... Arguments passed to \link[glue]{glue}
#'
#' @return character vector
#' @importFrom glue glue
#' @export
#'
#' @examples
#'  glueb("Hello {name}", name = "world")
glueb <- function(x, ...){
  glue::glue(x, ..., .envir = parent.frame(), .open = "[", .close = "]")
}
