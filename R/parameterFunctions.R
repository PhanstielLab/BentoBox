## Define a function for `bb_params` and parameter parsing logic
# @param params bb_params object to override default arguments of 
# parent function
# @param defaultArgs List of defaults for each argument of parent function
# @param declaredArgs List of arguments to override all others
# @param class Name of internal function class
parseParams <- function(params = params,
                        defaultArgs = formals(eval(match.call()[[1]])),
                        declaredArgs = lapply(match.call()[-1], 
                                            eval.parent, n = 2),
                        class) {
    ## Remove 'params' and '...' from defaultArgs and declaredArgs
    defaultArgs[["params"]] <- NULL
    declaredArgs[["params"]] <- NULL
    
    if ("..." %in% names(defaultArgs)){
        defaultArgs[["..."]] <- NULL
    }
    if ("..." %in% names(declaredArgs)){
        declaredArgs[["..."]] <- NULL
    }
    
    ## If bb_params are supplied override matching defaultArguments
    if (!is.null(params)) {
        if (class(params) == "bb_params") {
            ## Replace matching defaultArgs with params
            matchedParams <- params[na.omit(sort(match(names(defaultArgs), 
                                                            names(params))))]
            defaultArgs[na.omit(match(names(params), 
                                    names(defaultArgs)))] <- matchedParams
        } else {
            warning("Input object ignored. Object must be a",
                    " \'bb_params\' class object.", call. = FALSE)
        }
    }
    
    ## Replace default args with declared args
    if (length(declaredArgs) != 0) {
        suppressWarnings(defaultArgs[names(defaultArgs) 
                                    %in% names(declaredArgs)] <- declaredArgs)
    }
    ## Set arguments without default to NULL
    unset <- unlist(lapply(defaultArgs, is.name))
    defaultArgs[unset] <- lapply(lapply(defaultArgs[unset], deparse), as.null)
    
    ## Add arguments to object and evaluate
    object <- structure(.Data = defaultArgs,
                        class = class)
    object <- lapply(object, eval, rlang::ns_env("BentoBox"))
    
    return(object)
}

## Define a function that parses gpar parameters
# @param gpList Input gpar() class list 
# @param params Internal function object
# @param ... Additional arguments passed in from parent function call
setGP <- function(gpList, params, ...) {
    availGPs <- names(get.gpar())
    gpMatches <- params[which(names(params) %in% availGPs)]
    gpList[names(gpMatches)] <- gpMatches
    gpList[names(list(...))] <- list(...)
    
    ## Reset with fontface first
    if ("fontface" %in% names(gpList)) {
        otherParams <- gpList
        gpList <- gpar(fontface = gpList$fontface)
        gpList[names(otherParams)] <- otherParams
    }
    
    return(gpList)
}

## Define a function that converts coordinates/dimensions 
## for standard plot objects into default units
# @param object Function object containing x, y, width, height valus
# @param default.units String value of default.units
defaultUnits <- function(object, default.units) {
    if (!(is.null(object$x)) & !(is.null(object[["y"]]))) {
        if (!"unit" %in% class(object$x)) {
            if (!is.numeric(object$x)) {
                stop("x-coordinate is neither a unit object nor a ",
                    "numeric value. Cannot place object.", call. = FALSE)
            }
            
            if (is.null(default.units)) {
                stop("x-coordinate detected as numeric.\'default.units\' ",
                    "must be specified.", call. = FALSE)
            }
            
            object$x <- unit(object$x, default.units)
        }
        
        
        if (!"unit" %in% class(object$y)) {
            
            ## Check for "below" y-coord
            if (grepl("b", object$y) == TRUE) {
                if (grepl("^[ac-zA-Z]+$", object$y) == TRUE) {
                    stop("\'below\' y-coordinate detected with additional ",
                        "letters. Cannot parse y-coordinate.", call. = FALSE)
                }
                
                if (is.na(as.numeric(gsub("b", "", object$y)))) {
                    stop("\'below\' y-coordinate does not have a numeric ",
                        "associated with it. Cannot parse y-coordinate.",
                        call. = FALSE
                    )
                }
                
                object$y <- plot_belowY(y_coord = object$y)
            } else {
                if (!is.numeric(object$y)) {
                    stop("y-coordinate is neither a unit object nor a ",
                        "numeric value. Cannot place plot.", call. = FALSE)
                }
                
                if (is.null(default.units)) {
                    stop("y-coordinate detected as numeric. \'default.units\' ",
                        "must be specified.", call. = FALSE)
                }
                
                object$y <- unit(object$y, default.units)
            }
        }
        
        if (!"unit" %in% class(object$width)) {
            if (!is.numeric(object$width)) {
                stop("width is neither a unit object nor a numeric value. ",
                    "Cannot place plot.", call. = FALSE)
            }
            
            if (is.null(default.units)) {
                stop("width detected as numeric. \'default.units\' must ",
                    "be specified.",
                    call. = FALSE
                )
            }
            
            object$width <- unit(object$width, default.units)
        }
        
        if (!"unit" %in% class(object$height)) {
            if (!is.numeric(object$height)) {
                stop("height is neither a unit object nor a numeric ",
                    "value. Cannot place plot.", call. = FALSE)
            }
            
            if (is.null(default.units)) {
                stop("height detected as numeric.v\'default.units\' ",
                    "must be specified.",
                    call. = FALSE
                )
            }
            
            object$height <- unit(object$height, default.units)
        }
    }
    
    return(object)
}

## Define a function that converts coordinates/dimensions 
## for other miscellaneous elements into default units
# @param value value to be checked/converted
# @param name string name identifying king of value
# @param default.units String value of default.units
# @param funName String value of function name called within
# @param yBelow Logical indicating whether to allow "below" y coords
misc_defaultUnits <- function(value, name, default.units, 
                            funName = NULL, yBelow = TRUE){
    
    if (!"unit" %in% class(value)){
        ## Check for y
        if (grepl("y", name)){
            ## Check for "below" y-coord
            if (all(grepl("b", value))){
                if (yBelow == FALSE){
                    stop("\'below\' ", name, "-coordinate detected. Cannot ",
                        "parse \'below\' ", name, "-coordinate for ", 
                        funName, ".",
                        call. = FALSE)
                } else {
                    ## "below" y-coord additional letters
                    if (any(grepl("^[ac-zA-Z]+$", value))){
                        stop("\below\' ", name, "-coordinate detected with ",
                            "additional letters. Cannot parse ",
                            name, "-coordinate.", call. = FALSE)
                    }
                    ## "below" y-coord missing a number
                    if (any(is.na(as.numeric(gsub("b", "", value))))){
                        stop("\below\' ", name, "-coordinate does not have ",
                            "a numeric associated with it. Cannot parse ",
                            name, "-coordinate.", call. = FALSE)
                    }
                    
                    value <- unit(unlist(lapply(value, plot_belowY)),
                                get("page_units", envir = bbEnv))
                    
                }
            } else {
                ## y-coord, not detected as "below"
                if (!is.numeric(value)){
                    stop(name, "-coordinate is neither a unit object nor a ",
                        "numeric value. Cannot place plot.", call. = FALSE)
                }
                if (is.null(default.units)){
                    stop(name, "-coordinate detected as numeric. ",
                        "\'default.units\' must be specified.", call. = FALSE)
                }
                value <- unit(value, default.units)
            }
            
            
        } else {
            
            if (grepl("x", name)){
                name <- paste0(name, "-coordinate")
            }
            
            if (!is.numeric(value)){
                stop(name, " is neither a unit object nor a ",
                    "numeric value. Cannot place plot.", call. = FALSE)
            }
            if (is.null(default.units)){
                stop(name, " detected as numeric. ",
                    "\'default.units\' must be specified.", call. = FALSE)
            }
            value <- unit(value, default.units) 
        }
        
        
        
        
    }
    
    return(value)
}