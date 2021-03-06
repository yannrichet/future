#' Options used for futures
#'
#' Below are all \R options that are currently used by the \pkg{future} package and packages enhancing it.\cr
#' \cr
#' \emph{WARNING: Note that the names and the default values of these options may change in future versions of the package.  Please use with care until further notice.}
#'
#' @section Options for controlling futures:
#' \describe{
#'  \item{\option{future.plan}:}{Default future strategy plan used unless otherwise specified via \code{\link{plan}()}. This will also be the future plan set when calling \code{plan("default")}.  If not specified, this option may be set when the \pkg{future} package is \emph{loaded} if command-line option \code{--parallel=ncores} (short \code{-p ncores}) is specified; if \code{ncores > 1}, then option \option{future.plan} is set to \code{multiprocess} otherwise \code{sequential} (in addition to option \option{mc.cores} being set to \code{ncores-1}, if \code{ncores >= 1}).  If system environment variable \env{R_FUTURE_PLAN} is set, then that overrides the future plan set by the command-line option. (Default: \code{sequential})}
#'  \item{\option{future.globals.onMissing}:}{Action to take when non-existing global variables ("globals" or "unknowns") are identified when the future is created.  If \code{"error"}, an error is generated immediately.  If \code{"ignore"}, no action is taken and an attempt to evaluate the future expression will be made.  The latter is useful when there is a risk for false-positive globals being identified, e.g. when future expression contains non-standard evaluation (NSE).  (Default: \code{"error"}; likely to change to \code{"ignore"} in a future release)}
#'  \item{\option{future.globals.method}:}{Method used to identify globals. For details, see \code{\link[globals]{globalsOf}()}. (Default: \code{"ordered"})}
#'  \item{\option{future.globals.maxSize}:}{Maximum allowed total size (in bytes) of global variables identified. Used to prevent too large exports. (Default: \code{500*1024^2} = 500 MiB)}
#'  \item{\option{future.globals.resolve}:}{If \code{TRUE}, globals that are \code{\link{Future}} objects (typically created as \emph{explicit} futures) will be resolved and have their values (using \code{value()}) collected.  Because searching for unresolved futures among globals (including their content) can be expensive, the default is not to do it and instead leave it to the run-time checks that assert proper ownership when resolving futures and collecting their values. (Default: \code{FALSE})}
#'  \item{\option{future.resolve.recursive}:}{An integer specifying the maximum recursive depth to which futures should be resolved. If negative, nothing is resolved.  If \code{0}, only the future itself is resolved.  If \code{1}, the future and any of its elements that are futures are resolved, and so on. If \code{+Inf}, infinite search depth is used. (Default: \code{0})}
#'  \item{\option{future.wait.timeout}:}{Maximum waiting time (in seconds) for a free worker before a timeout error is generated. (Default: \code{30*24*60*60} (= 30 days))}
#'  \item{\option{future.wait.interval}:}{Initial interval (in seconds) between polls. (Default: \code{0.2} (0.2 seconds))}
#'  \item{\option{future.wait.alpha}:}{Positive scale factor used to increase the interval after each poll. (Default: \code{1.01})}
#' }
#'
#' @section Options for debugging futures:
#' \describe{
#'  \item{\option{future.debug}:}{If \code{TRUE}, extensive debug messages are generated. (Default: \code{FALSE})}
#'  \item{\option{future.progress}:}{If \code{TRUE}, progress is displayed while blocked waiting for futures to be resolved. (Default: \code{FALSE})}
#' }
#'
#' @section Options for controlling package startup:
#' \describe{
#'  \item{\option{future.startup.script}:}{If \code{FALSE}, any \file{.future.R} startup scripts, which may exist in the current directory and / or the user's home directory, are ignored. Such startup scripts are otherwise sourced when the \pkg{future} package is \emph{attached}. \emph{Importantly}, this option is \emph{always} set to \code{FALSE} if the \pkg{future} package is loaded as part of a future expression being evaluated, e.g. in a background process. In order words, they are sourced in the main R process but not in future processes. (Default: \code{TRUE} in main R process and \code{FALSE} in future processes / during future evaluation)}
#'  \item{\option{future.cmdargs}:}{Overrides \code{\link[base]{commandArgs}()} when the \pkg{future} package is \emph{loaded}.}
#' }
#'
#' @section Options for configuring low-level system behaviors:
#' \describe{
#'  \item{\option{future.availableCores.methods}:}{Default lookup methods for \code{\link{availableCores}()}. (Default: \code{c("system", "mc.cores+1", "_R_CHECK_LIMIT_CORES_", "PBS", "SGE", "Slurm", "fallback")})}
#'
#'  \item{\option{future.availableWorkers.methods}:}{Default lookup methods for \code{\link{availableWorkers}()}. (Default: \code{c("mc.cores+1", "_R_CHECK_LIMIT_CORES_", "PBS", "SGE", "Slurm", "system", "fallback")})}
#'
#'  \item{\option{future.availableCores.fallback}:}{Number of cores to use when no core-specifying settings are detected other than \code{"system"}. If not specified, this option is set according to system environment variable \env{R_FUTURE_AVAILABLECORES_FALLBACK} when the \pkg{future} package is \emph{loaded}. This options makes it possible to set the default number of cores returned by \code{availableCores()} / \code{availableWorkers()} yet allow users and schedulers to override it. In HPC environment, it is useful to set \code{R_FUTURE_AVAILABLECORES_FALLBACK=1}.}
#' 
#'  \item{\option{future.availableCores.system}:}{Number of "system" cores used instead of what is reported by \code{\link{availableCores}(which="system")}. If not specified, this option is set according to system environment variable \env{R_FUTURE_AVAILABLECORES_SYSTEM} when the \pkg{future} package is \emph{loaded}. This option allows you to effectively override what \code{parallel::detectCores()} reports the system has.}
#' }
#'
#' @section Options for demos:
#' \describe{
#'  \item{\option{future.demo.mandelbrot.region}:}{Either a named list of \code{\link{mandelbrot}()} arguments or an integer in [1,3] specifying a predefined Mandelbrot region. (Default: \code{1L})}
#'  \item{\option{future.demo.mandelbrot.nrow}:}{Number of rows and columns of tiles. (Default: \code{3L})}
#' }
#'
#' @seealso
#' To set \R options when \R starts (even before the \pkg{future} package is loaded), see the \link[base]{Startup} help page.  The \href{https://cran.r-project.org/package=startup}{\pkg{startup}} package provides a friendly mechanism for configurating \R's startup process.
#'
#' @aliases future.availableCores.methods future.cmdargs future.cores future.debug future.globals.maxSize future.globals.method future.globals.onMissing future.globals.resolve future.plan future.progress future.resolve.recursive future.startup.script future.wait.alpha future.wait.interval future.wait.times R_FUTURE_PLAN R_FUTURE_CORES future.demo.mandelbrot.region future.demo.mandelbrot.nrow
#' @keywords internal
#' @name future.options
NULL
