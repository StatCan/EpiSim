#' Main box/compartment model functions
#'
#' @export
#'
#' @importFrom adaptivetau ssa.adaptivetau
#' @importFrom deSolve lsoda
#' @importFrom dplyr arrange filter is.tbl mutate select slice
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#'
#' @param file.name a character element.
#' @param sheets.names a list.
#' @param just.get.functions a logical element (by default, FALSE).
#' @param functions.kit a list containing one or more of the following: $differential.eqns.func, $post.processing.func, $post.processing.companion.kit, $CTMC.eqns.func, $CTMC.eqns.func.companion (by default, NULL).
#' @param also.get.flows a character element (by default, NULL).
#' @param agegrp.glue a character element (by default, an empty character element).
#' @param CTMC.random.seeds an integer vector (by default, NULL).
#'
#' @examples
#'
#' # Get full path to demo model (in Excel workbook)
#' # that comes with the EpiSim package
#' model.1.workbook <- get.path("demo.model.1.xls")
#'
#' # Define workbook sheet names
#' sheet.names <- list(
#'   parms.notime.0d = "Parameters any time any age",
#'   parms.0d = "Parameters any age",
#'   parms.1d = "Parameters by Age",
#'   parms.2d = "Parameters by Age x Age",
#'   initial.conditions = "Initial conditions",
#'   model.flow = "Model Specs (not lazy v1)",
#'   auxiliary.vars = "Intermediate calculations",
#'   post.processing = "Post Processing Empty"
#' )
#'
#' # Fit model with model.flow = "Model Specs (not lazy v1)"
#' results.1 <- seir.n.age.classes(model.1.workbook, sheet.names)
#'
#' @return a list containing the results of the box/compartment model.

seir.n.age.classes = function(file.name, sheets.names, just.get.functions=FALSE, functions.kit=NULL,
                              also.get.flows=NULL, agegrp.glue="", CTMC.random.seeds=NULL) # agegrp.glue=".ag"
{
  # functions.kit may be NULL or a list containing SOME of the following
  #   $differential.eqns.func
  #   $post.processing.func
  #   $post.processing.companion.kit
  #   $CTMC.eqns.func
  #   $CTMC.eqns.func.companion

  if(is.null(functions.kit$CTMC.eqns.func.companion) != is.null(functions.kit$CTMC.eqns.func))
    stop("\n CTMC.eqns.func and CTMC.eqns.func.companion must be both provided or both omitted")

  expected.sheets.names = c("parms.notime.0d","parms.0d","parms.1d","parms.2d","initial.conditions","model.flow","auxiliary.vars")

  if( !all( expected.sheets.names %in% names(sheets.names) ) )
    stop( paste("\n sheets.names must provide the following:",paste(expected.sheets.names,collapse=", ")) )


  stop.if.tbl = function(x)
  {
    if(is.tbl(x))
      stop (paste("\n tbl object detected:" , deparse(substitute(x)) ))
  }


  input.info.verbatim = sheets.names
  input.info.verbatim$file.name = file.name
  input.info.verbatim$agegrp.glue = agegrp.glue
  input.info.verbatim$CTMC.random.seeds = CTMC.random.seeds
  #==========================================================================
  #   Initial conditions and parameters
  #==========================================================================

  tmp_parm_notime_0d = sheets.names$parms.notime.0d     # 0 dimensional parameters (no age, no time)
  tmp_parm_0d        = sheets.names$parms.0d            # 0 dimensional parameters (no age       )
  tmp_parm_1d        = sheets.names$parms.1d            # 1 dimensional parameters (   age       )
  tmp_parm_2d        = sheets.names$parms.2d            # 2 dimensional parameters (   age x age )
  input_stuff        = sheets.names$initial.conditions  # initial values/conditions

  if(!is.data.frame(tmp_parm_notime_0d))
    tmp_parm_notime_0d <- as.data.frame ( readxl::read_excel(file.name, sheet = tmp_parm_notime_0d) )  # 0 dimensional parameters

  if(!is.data.frame(tmp_parm_0d))
    tmp_parm_0d <- as.data.frame ( readxl::read_excel(file.name, sheet = tmp_parm_0d) )  # 0 dimensional parameters

  if(!is.data.frame(tmp_parm_1d))
    tmp_parm_1d <- as.data.frame ( readxl::read_excel(file.name, sheet = tmp_parm_1d) )  # 1 dimensional parameters

  if(!is.data.frame(tmp_parm_2d))
    tmp_parm_2d <- as.data.frame ( readxl::read_excel(file.name, sheet = tmp_parm_2d) ) # 2 dimensional parameters

  if(!is.data.frame(input_stuff))
    input_stuff  <- as.data.frame ( readxl::read_excel(file.name, sheet = input_stuff) ) # initial values/conditions

  stop.if.tbl(tmp_parm_notime_0d)
  stop.if.tbl(tmp_parm_0d)
  stop.if.tbl(tmp_parm_1d)
  stop.if.tbl(tmp_parm_2d)
  stop.if.tbl(input_stuff      )

  raw.init.conditions = input_stuff # keep snapshot for output.  input_stuff may be altered later

  init.cond.numeric.vars = setdiff(colnames(input_stuff),"NAME")

  init_list <- list() # Same content as input_stuff but in a list format
  for(k in input_stuff$NAME)
    init_list[[k]] <- as.matrix( subset(input_stuff, NAME == k)[,init.cond.numeric.vars] )


  nagegrp <- length(unique(tmp_parm_1d$agegrp)) # number of age groups
  nagegrp = ncol(raw.init.conditions) - 1       # number of age groups

  #BEGIN get raw.compartments.age and pretty.compartments.age
  agegrp.suffix.pretty = agegrp.suffix = ""
  if(nagegrp > 1)
  {
    agegrp.suffix        = 1:nagegrp
    agegrp.suffix.pretty = paste0(agegrp.glue,1:nagegrp) # if agegrp.glue = ".ag" this generates ".ag1" ".ag2" ".ag3"
  }

  raw.compartments =   raw.init.conditions$NAME # e.g.  S D L
  raw.compartments.age = c ( t( outer( raw.compartments,agegrp.suffix       ,paste0) ) ) # e.g. S1    S2    S3    S4    S5    D1    D2    ...
  pretty.compartments.age = c ( t( outer( raw.compartments,agegrp.suffix.pretty,paste0) ) ) # e.g. S.ag1 S.ag2 S.ag3 S.ag4 S.ag5 D.ag1 D.ag2 ...

  if(length(unique(raw.compartments.age)) != length(raw.compartments.age)  )
    stop("\n Ambuiguity from compartment names and ages")
  # Ambiguity could arise if there are 2 compartments named S and S1 (say) and there are > 10 age groups
  # Then S11 could be 11th age group of S or first age group of S1
  #END get raw.compartments.age and pretty.compartments.age

  nrow_   <- dim(tmp_parm_1d)[1]/nagegrp

  parm_value_notime_0d = tmp_parm_notime_0d

  parm_value_0d <- dplyr::arrange(tmp_parm_0d, tmin        ) # sort by tmin
  parm_value_0d$isim = 1:nrow_  # (1:nrow(parm_value_0d))

  parm_value_1d <- dplyr::arrange(tmp_parm_1d, tmin, agegrp) # sort by tmin agegrp
  parm_value_1d <- parm_value_1d %>%
    mutate(isim = rep(1:nrow_, each=nagegrp))

  parm_value_2d <- dplyr::arrange(tmp_parm_2d, tmin, cagegrp, ragegrp) # sort by tmin cagegrp  ragegrp
  parm_value_2d <- parm_value_2d %>%
    mutate(isim = rep(1:nrow_, each = nagegrp*nagegrp))


  nTimeSegments <- max(parm_value_1d$isim)
  CTMC.parms.info = list(tmin.vect=sort(unique(parm_value_1d$tmin)),parms.0d=list(),parms.1d=list(),parms.2d=list())

  segments.labels = c()
  for(segment in seq(1, nTimeSegments, 1))
  {

    #   CTMC.parms.info$parms.0d[[segment]] = subset(parm_value_0d, isim == segment) # also want to drop isim
    #   CTMC.parms.info$parms.1d[[segment]] = subset(parm_value_1d, isim == segment)
    #   CTMC.parms.info$parms.2d[[segment]] = subset(parm_value_2d, isim == segment)
    CTMC.parms.info$parms.0d[[segment]] = parm_value_0d %>% filter(isim == segment) %>% select(-isim)
    CTMC.parms.info$parms.1d[[segment]] = parm_value_1d %>% filter(isim == segment) %>% select(-isim)
    CTMC.parms.info$parms.2d[[segment]] = parm_value_2d %>% filter(isim == segment) %>% select(-isim)

    segments.labels = c(segments.labels,paste(CTMC.parms.info$parms.0d[[segment]][,c("tmin","tmax")],collapse = "-->") )

    CTMC.parms.info$parms.0d[[segment]] = cbind(CTMC.parms.info$parms.0d[[segment]] , parm_value_notime_0d)

    # Right now CTMC.parms.info$parms.2d[[segment]] is data.frame (akin to CTMC.parms.info$parms.0d[[segment]] and CTMC.parms.info$parms.1d[[segment]] )
    # Consider Changing CTMC.parms.info$parms.2d[[segment]] from data.frame to list of matrices
    # look for temp[cbind(age.age.parms$ragegrp, age.age.parms$cagegrp)]
  }
  names(CTMC.parms.info$parms.0d) = segments.labels
  names(CTMC.parms.info$parms.1d) = segments.labels
  names(CTMC.parms.info$parms.2d) = segments.labels

  rm(parm_value_notime_0d, parm_value_0d, parm_value_1d, parm_value_2d, segments.labels)


  #===================================================================
  # Build function eval.post.processing.func (if not provided)
  #===================================================================

  code.body.df = sheets.names$post.processing # data.frame or string
  if(!is.data.frame(code.body.df))
    code.body.df = as.data.frame ( readxl::read_excel(file.name, sheet = code.body.df ) )

  stop.if.tbl(code.body.df)

  code.body.df$code[is.na(code.body.df$code)] = ""

  post.processing.companion.kit = functions.kit$post.processing.companion.kit
  eval.post.processing.func     = functions.kit$post.processing.func
  if(is.null(eval.post.processing.func))
  {
    code.body.char = code.body.df$code
    code.head.char = c("function(list.solution.etcetera){",
                       "# list.solution.etcetera contains $solution and possibly $solution.inflows and/or $solution.outflows" ,
                       "# Probably could use    with(list.solution.etcetera)   instead of copying" ,
                       "solution            = list.solution.etcetera$solution",
                       "solution.inflows    = list.solution.etcetera$solution.inflows",
                       "solution.outflows   = list.solution.etcetera$solution.outflows",
                       "CTMC.df             = list.solution.etcetera$CTMC.out$CTMC.df",
                       "parms.notime.0d     = list.solution.etcetera$input.info$parms.notime.0d",
                       "parms.0d            = list.solution.etcetera$input.info$parms.0d",
                       "parms.1d            = list.solution.etcetera$input.info$parms.1d",
                       "parms.2d            = list.solution.etcetera$input.info$parms.2d",
                       "initial.conditions  = list.solution.etcetera$input.info$initial.conditions",
                       "companion.kit       = list.solution.etcetera$functions$post.processing.companion.kit",
                       "input.info.verbatim = list.solution.etcetera$input.info.verbatim",
                       "sommaire            = data.frame(osqvhfipumzcdjblkwrgnexaty=4)" ,
                       "## BEGIN code from excel"  )
    code.tail.char = c("## END code from excel",
                       "sommaire$osqvhfipumzcdjblkwrgnexaty=c()",
                       "list.solution.etcetera$solution = solution # only solution may be modified",
                       "list.solution.etcetera$sommaire = sommaire # add on new kid on the block",
                       "list.solution.etcetera",
                       "#list(sommaire=sommaire,solution.inflows=solution.inflows,solution.outflows=solution.outflows)" )

    eval.post.processing.func = paste(c(code.head.char , code.body.char , code.tail.char , "}"),collapse="\n" )
    eval.post.processing.func = eval(parse(text= eval.post.processing.func )) # from text to function
  }

  #===================================================================
  # Build model_flows_tmp and auxiliary.vars
  #===================================================================

  model_flows_tmp = sheets.names$model.flow
  auxiliary.vars  = sheets.names$auxiliary.vars

  if(!is.data.frame(auxiliary.vars))
    auxiliary.vars   <- as.data.frame ( readxl::read_excel(file.name, sheet = auxiliary.vars ) )  # auxiliary variables in equations

  stop.if.tbl(auxiliary.vars)
  auxiliary.vars = subset(auxiliary.vars, !is.na(code)   )  # auxiliary.vars$code will be top portion of with(...,{ ... })  part


  if(!is.data.frame(model_flows_tmp))
    model_flows_tmp  <- as.data.frame ( readxl::read_excel(file.name, sheet = model_flows_tmp) )  # arrows in flowchart

  stop.if.tbl(model_flows_tmp)
  model_flows_tmp = subset( model_flows_tmp,!is.na(expression) & activation ==1)

  #===================================================================
  # Build function eval.differential.eqns.func (if not provided)
  #===================================================================

  eval.differential.eqns.func = functions.kit$differential.eqns.func
  ratefunc.for.adaptivetau    = functions.kit$CTMC.eqns.func
  transitions.for.adaptivetau = functions.kit$CTMC.eqns.func.companion
  flows.requested = !is.null(also.get.flows) && any(also.get.flows %in% c("inflows","outflows"))
  if(flows.requested || is.null(eval.differential.eqns.func) || is.null(ratefunc.for.adaptivetau))
  {
    model_flows = model_flows_tmp     # keep model_flows_tmp for output. Work on model_flows from this point on

    # BEGIN update model_flows, input_stuff and init_list as per requested flows (if any)

    silly.inflow.model = silly.outflow.model = list()

    #if(!is.null(flows.prefix$inflows))
    if(any(also.get.flows == "inflows"))
      silly.inflow.model = get.silly.model.chunk(model_flows, input_stuff, init.cond.numeric.vars, "inflow" ,  "SlackBoxForInflows", "inflows.")

    #if(!is.null(flows.prefix$outflows))
    if(any(also.get.flows == "outflows"))
      silly.outflow.model= get.silly.model.chunk(model_flows, input_stuff, init.cond.numeric.vars, "outflow", "outflows.", "SlackBoxForOutflows")

    model_flows = rbind(model_flows, silly.inflow.model$model, silly.outflow.model$model)
    input_stuff = rbind(input_stuff, silly.inflow.model$init , silly.outflow.model$init )

    # Get init_list AGAIN.  This overwrites version created early on
    init_list <- list() # Same content as input_stuff but in a list format
    for(k in input_stuff$NAME)
      init_list[[k]] <- as.matrix( subset(input_stuff, NAME == k)[,init.cond.numeric.vars] )

    # END   update model_flows, input_stuff and init_list as per requested flows (if any)




    # BEGIN Build character vector differential.eqns.char  (and ratefunc.for.adaptivetau.chunks + transitions.for.adaptivetau )
    model_flows$multiplier = model_flows$From

    lazy.range=range(model_flows$lazy)
    if(is.na(lazy.range[1]))
      stop("\nstyle (lazy or not) must be specified for all transitions")
    if(diff(lazy.range)>0 || !(lazy.range[1] %in% c(0,1)) )  # may be relaxed later
      stop("\nstyle (lazy or not) should be all zeros or all ones")
    if(flows.requested && lazy.range[1]==1)
      stop("\nLazy style should not be used when inflows and/or outflows are requested.")  # may be relaxed later

    if(lazy.range[1] == 0)  # not lazy
      model_flows$multiplier = NA

    model_flows = model_flows %>%
      mutate(fromstar = ifelse(is.na(multiplier),"",paste0(From,"*(")) ,
             closing.parenthesis = ifelse(is.na(multiplier),"",")") ,
             expression = paste0(fromstar,expression,closing.parenthesis) )

    boxes = unique(c(model_flows$From,model_flows$To))
    outflows=rep("#####$$$$$$$$$$$!@@@@@@@@",length(boxes))
    names(outflows) = boxes
    inflows=outflows
    ratefunc.for.adaptivetau.chunks = c()
    transitions.for.adaptivetau = list() # overwritten by functions.kit$CTMC.eqns.func.companion if not NULL

    for(k in seq(nrow(model_flows)))
    {
      this.expression = model_flows$expression[k]
      outflows[model_flows$From[k]] = paste(outflows[model_flows$From[k]],"+", this.expression)
      inflows [model_flows$To[k]  ] = paste( inflows[model_flows$To[k]  ],"+", this.expression)

      ratefunc.for.adaptivetau.chunks = c(ratefunc.for.adaptivetau.chunks,this.expression)
      transitions.for.adaptivetau.chunk = c(-1,+1)
      names(transitions.for.adaptivetau.chunk) = model_flows[k,c("From","To")]
      transitions.for.adaptivetau[[k]] = transitions.for.adaptivetau.chunk
    }
    names(transitions.for.adaptivetau) = paste(model_flows$From,'-->',model_flows$To)
    names(ratefunc.for.adaptivetau.chunks) = names(transitions.for.adaptivetau)

    if(!is.null(functions.kit$CTMC.eqns.func.companion))
      transitions.for.adaptivetau = functions.kit$CTMC.eqns.func.companion

    differential.eqns.char = paste("(flow.multiplier$inflow)*(",inflows,") + (flow.multiplier$outflow)*(",outflows,")")
    differential.eqns.char = gsub(  "#####$$$$$$$$$$$!@@@@@@@@ +","" ,differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = gsub("( #####$$$$$$$$$$$!@@@@@@@@ )","0",differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = paste0("derivative.",boxes,"=",differential.eqns.char)

    #if( any(grepl("#####$$$$$$$$$$$!@@@@@@@",differential.eqns.char)) )  browser()


    rm(model_flows,boxes,inflows,outflows,this.expression) # clean up intermediate objects no longer required

    # END Build character vector differential.eqns.char  (and ratefunc.for.adaptivetau.chunks + transitions.for.adaptivetau )


    # BEGIN Build function eval.differential.eqns.func

    ODE.func.header.char = c("function(list.boxes, list.parms, time.now, flow.multiplier=list(inflow=1,outflow=-1)){" ,"#browser() #not a good place for it")
    CTMC.func.header.char = c("function(list.boxes, list.parms, time.now) {"                                           ,"#browser() #not a good place for it")  # , "print(time.now)"

    # BEGIN get ODE.within.with.part.char and CTMC.within.with.part.char
    compartments =   input_stuff$NAME # e.g.  S D L   (note it is not be the same as raw.compartments if also.get.flows != NULL)
    compartments.age = c ( t( outer( compartments,seq(nagegrp),paste0) ) ) # e.g. S1 S2 S3 S4 S5 D1 D2 ...
    out.char      = paste0("out=c(", paste0("derivative.", paste(compartments    , collapse=", derivative.")),")")
    names.out.char= paste0("out=c(", paste0("derivative.", paste(compartments.age, collapse=",derivative." )),")")
    #names.out.char = gsub(",","','"  ,names.out.char ,fixed=TRUE) # replace , by ','     may generate a line that is too long causing problems
    names.out.char = gsub(",","',\n'",names.out.char ,fixed=TRUE) # replace , by ',\n'   invoids the problematic single line that is too long
    names.out.char = gsub("(","('"   ,names.out.char ,fixed=TRUE) # replace ( by ('
    names.out.char = gsub(")","')"   ,names.out.char ,fixed=TRUE) # replace ) by ')
    names.out.char = gsub("out=","names(out)=",names.out.char ,fixed=TRUE)

    ODE.within.with.part.char = paste(c(differential.eqns.char,"#browser()",out.char,names.out.char,"#print(sum(out));#print(cbind(out));#browser()","list(out)"),collapse="\n") # First tried ";\n" but realised that "\n" is sufficient

    CTMC.within.with.part.char = paste("c(", paste(ratefunc.for.adaptivetau.chunks, collapse=" ,\n "),")")
    # END get ODE.within.with.part.char and CTMC.within.with.part.char

    ODE.with.char = c("relevant.parm=sum(time.now >= list.parms$tmin.vect)",
                      "names(time.now     )='time.now'",
                      "## list.parms = as.list(c(list.parms['tmin.vect'], list.parms$parms.0d[[relevant.parm]], list.parms$parms.1d[[relevant.parm]] , list.parms$parms.2d[[relevant.parm]])) # for CTMC.with.char",
                      "list.parms=as.list(c( time.now, list.parms ))" ,
                      "with(as.list(c(list.boxes, list.parms)),{",
                      "#browser() # a good place for it" )
    # ODE.with.char = gsub("list.parms$parms.1d[[relevant.parm]] , list.parms$parms.2d[[relevant.parm]]","list.parms",CTMC.with.char,fixed=TRUE)
    CTMC.with.char = gsub("##","",ODE.with.char,fixed=TRUE)


    # BEGIN Create functions eval.differential.eqns.func and ratefunc.for.adaptivetau
    tmp.eval.differential.eqns.func = paste(c( ODE.func.header.char,  ODE.with.char, auxiliary.vars$code,  ODE.within.with.part.char,"})" ,"}"),collapse="\n" ) # code of eval.differential.eqns.func
    tmp.ratefunc.for.adaptivetau    = paste(c(CTMC.func.header.char, CTMC.with.char, auxiliary.vars$code, CTMC.within.with.part.char,"})" ,"}"),collapse="\n" ) # code of ratefunc.for.adaptivetau

    # cat(eval.differential.eqns.func) # quick look at how it look like
    if(is.null(eval.differential.eqns.func))
      eval.differential.eqns.func = eval(parse(text = tmp.eval.differential.eqns.func ))

    if(is.null(ratefunc.for.adaptivetau   ))
      ratefunc.for.adaptivetau    = eval(parse(text = tmp.ratefunc.for.adaptivetau    ))
    # END Create functions eval.differential.eqns.func and ratefunc.for.adaptivetau


    rm(CTMC.within.with.part.char, CTMC.func.header.char, CTMC.with.char,
       ODE.within.with.part.char,  ODE.func.header.char,  ODE.with.char, out.char) # clean up
    # END Build function eval.differential.eqns.func
  }

  resultat = list(functions=list( differential.eqns.func    = eval.differential.eqns.func ,
                                  CTMC.eqns.func            =    ratefunc.for.adaptivetau,
                                  CTMC.eqns.func.companion  = transitions.for.adaptivetau,
                                  post.processing.func      = eval.post.processing.func,
                                  post.processing.companion.kit = post.processing.companion.kit ))

  if(just.get.functions)
    return ( resultat$functions )

  #==========================================================================
  #  Main routine
  #==========================================================================
  # The SEIR model with N age classes
  #
  SEIR.n.Age.Classes.within.loop <- function( time=NULL, tmin.vect=NULL, ageless.parms = NULL, age.parms = NULL, age.age.parms = NULL,list.inits = NULL, not.parms=  c("tmin", "tmax", "agegrp", "cagegrp", "ragegrp"))
  {
    if (is.null(ageless.parms))
      stop("undefined 'ageless.parms'")

    if (is.null(age.parms))
      stop("undefined 'age.parms'")

    if (is.null(age.age.parms))
      stop("undefined 'age.age.parms'")

    if (is.null(time))
      stop("undefined 'time'")

    if (is.null(list.inits))
      stop("undefined 'list.inits'")

    if (is.null(tmin.vect))
      stop("undefined 'tmin.vect'")

    nage = nrow(age.parms)

    list.parms.for.lsoda <- list(tmin.vect=tmin.vect)

    for(k in setdiff(names(ageless.parms), not.parms ))
      list.parms.for.lsoda[[k]] <- ageless.parms[,k]

    for(k in setdiff(names(age.parms), not.parms ))
      list.parms.for.lsoda[[k]] <- age.parms[,k]

    for(k in setdiff(names(age.age.parms), not.parms ))
    {
      temp<- array(NA, c(nage,nage))
      temp[cbind(age.age.parms$ragegrp, age.age.parms$cagegrp)] <- age.age.parms[,k]
      list.parms.for.lsoda[[k]] <- temp
      if(any(is.na(temp)))
        stop(paste0(k," matrix has some missing entries"))
    }

    ### to write the system of differential equations
    calculate_derivatives <- function(time, vec.inits, list.parms, names.inits) {


      iota <- seq(length(vec.inits)/length(names.inits))
      list.inits <- list()
      for(k in names.inits){
        if (length(iota) > 1) {
          list.inits[[k]] <- vec.inits[paste0(k, iota)]
        }else{list.inits[[k]] <- vec.inits[k] }
      }

      eval.differential.eqns.func(list.inits, list.parms, time)


    } #end of function calculate_derivatives

    ###---------------------------------------------------------------------------------
    ### Solver for Ordinary Differential Equations (ODE), Switching Automatically
    ### Between Stiff and Non-stiff Methods
    ###---------------------------------------------------------------------------------
    #browser()
    list.parms  = list.parms.for.lsoda
    output <-  lsoda(y = unlist(list.inits),
                     times = time,
                     func =  calculate_derivatives,
                     parms = list.parms,
                     names.inits = names(list.inits))

    return(output)
  }  # END  of function SEIR.n.Age.Classes.within.loop

  lean.ssa.adaptivetau <- function(sim_number, init.conditions, params,racines,etiquettes)
  {
    if(!is.na(racines[sim_number]))
      set.seed(racines[sim_number])

    #simu.all = ssa.exact ( init.values = init.conditions,  # need to remove argument tl.params
    simu.all = ssa.adaptivetau(init.values = init.conditions,
                               transitions = transitions.for.adaptivetau,
                               rateFunc = ratefunc.for.adaptivetau,
                               params = params,
                               tf = tmax,
                               tl.params = list(epsilon=0.01))

    temps = sort(c(simu.all[,"time"],seq(tmax-1)))
    int.whereabouts  = which(temps %in% 0:tmax)   # int.whereabouts  such that temps   [int.whereabouts] = 0:tmax
    int.whereabouts2 = int.whereabouts - (0:tmax) # int.whereabouts2 such that simu.all[int.whereabouts2,"time"][k+1] is largest time <= k
    simu.lean = as.data.frame(simu.all[int.whereabouts2,]) # snapshots at t= 0, 1, 2, ... , tmax
    simu.lean$time = 0:tmax
    simu.lean$what.is.it = etiquettes[sim_number]

    list(simu.all=simu.all,simu.lean=simu.lean)
  }
  ################################################################
  #        To run the example
  ################################################################

  # Should consider dropping the "for(segment in ...)" loop and call SEIR.n.Age.Classes.within.loop only once
  # This should be feasible as this is very much what is done with lean.ssa.adaptivetau

  excluded_names <- c("tmin", "tmax","agegrp","cagegrp","ragegrp")
  previous.tmax <- 0
  df.out = c()
  updated_init_list = init_list

  for(segment in seq(1, nTimeSegments, 1))
  {

    this.parameter.by.nothing <- CTMC.parms.info$parms.0d[[segment]]
    this.parameter.by.age     <- CTMC.parms.info$parms.1d[[segment]]
    this.parameter.by.age.age <- CTMC.parms.info$parms.2d[[segment]]

    tmin <- unique(c(this.parameter.by.nothing$tmin, this.parameter.by.age$tmin, this.parameter.by.age.age$tmin))
    tmax <- unique(c(this.parameter.by.nothing$tmax, this.parameter.by.age$tmax, this.parameter.by.age.age$tmax))


    if(length(tmin)>1 || length(tmax)>1 || tmin>=tmax )
      stop(paste0("Unexpected pattern in tmin, tmax for interval ", segment))

    # tt <- seq(0   , tmax - tmin, by = 1)
    tt <- seq(tmin, tmax       , by = 1)

    if(tmin != previous.tmax)
      stop(paste(interval.label , "\n  Interval lower bound not equal to previous interval upper bound"))



    previous.tmax <- tmax
    out <- SEIR.n.Age.Classes.within.loop( time=tt,
                                           tmin.vect = CTMC.parms.info$tmin.vect,
                                           ageless.parms = this.parameter.by.nothing,
                                           age.parms     = this.parameter.by.age,
                                           age.age.parms = this.parameter.by.age.age,
                                           list.inits = updated_init_list)

    out <- as.data.frame(out)
    # ode/lsoda Output diagnostic #######################
    #diagn <- diagnostics.deSolve(out)

    out$time <- seq(tmin,tmax,1)
    out_for_init <- out %>%
      slice(nrow(out)) %>%    # select last row
      pivot_longer(-time)     # fat to skinny
    init <- out_for_init$value
    names(init) <- out_for_init$name

    #   rowns <- names(select(out,-c(time)))
    #   out <- out %>%
    #     mutate(N_tot = rowSums(.[rowns]))  # Total number of individuals


    #updating the initial values
    for(k in 1:length(updated_init_list)){
      updated_init_list[[k]][1:nagegrp] <- init[seq(nagegrp*(k-1)+1,nagegrp*k)]
    }

    if(segment < nTimeSegments)
      out = out[-nrow(out),]
    # Add outputs to the list
    df.out = rbind(df.out,out)

  } #end for(segment in seq(1, nTimeSegments, 1))


  CTMC.out=NULL
  if(nagegrp == 1 && !is.null(CTMC.random.seeds))
  {
    iota.CTMC = seq(length(CTMC.random.seeds))
    CTMC.etiquettes = paste("CTMC sim",iota.CTMC)
    CTMC.list = lapply(iota.CTMC, FUN = lean.ssa.adaptivetau, unlist(init_list), CTMC.parms.info, CTMC.random.seeds, CTMC.etiquettes)
    names(CTMC.list) = CTMC.etiquettes
    CTMC.df = c()
    for(k in CTMC.etiquettes)
    {
      CTMC.df = rbind(CTMC.df,CTMC.list[[k]]$simu.lean)
      CTMC.list[[k]] = CTMC.list[[k]]$simu.all
    }
    CTMC.out=list(CTMC.list=CTMC.list,CTMC.df=CTMC.df)
  }

  #browser()
  if(!is.null(CTMC.out))
  {
    resultat$CTMC.out = CTMC.out
    #resultat$functions$CTMC.eqns.func = ratefunc.for.adaptivetau # always provide or not ?
  }

  #BEGIN add on $solution.inflows, $solution.outflows and $solution to resultat
  solution = df.out
  if(any(also.get.flows == "inflows"))
  {
    # var.names = c ( t( outer( paste0("inflows.",raw.compartments),agegrp.suffix,paste0) ) )
    var.names = paste0("inflows.",raw.compartments.age)
    resultat$solution.inflows  = solution[,c("time",var.names)]
    colnames( resultat$solution.inflows) = c("time",pretty.compartments.age)
  }
  if(any(also.get.flows == "outflows"))
  {
    #var.names = c ( t( outer( paste0("outflows.",raw.compartments),agegrp.suffix,paste0) ) )
    var.names = paste0("outflows.",raw.compartments.age)
    resultat$solution.outflows  = solution[,c("time",var.names)]
    resultat$solution.outflows[,-1] = - resultat$solution.outflows[,-1]
    colnames( resultat$solution.outflows) = c("time",pretty.compartments.age)
  }
  resultat$solution = solution[,c("time",raw.compartments.age)]
  colnames( resultat$solution) = c("time",pretty.compartments.age)
  #END add on $solution.inflows, $solution.outflows and $solution to resultat

  resultat$input.info = list(parms.notime.0d=tmp_parm_notime_0d , parms.0d=tmp_parm_0d, parms.1d=tmp_parm_1d, parms.2d=tmp_parm_2d, initial.conditions=raw.init.conditions,
                             auxiliary.vars=auxiliary.vars, model.flow=model_flows_tmp, post.processing=code.body.df)
  resultat$input.info.verbatim = input.info.verbatim

  eval.post.processing.func(resultat)
} #end of SEIR.n.Age.Classes function

#' Get box/compartment model flows
#'
#' @noRd
#'
#' @keywords Internal
#'
#' @param model_flows a character element.
#' @param init.cond a character element.
#' @param init.cond.numeric.vars a character vector.
#' @param which.flow a character element.
#' @param NewFrom a character element.
#' @param NewTo a character element.
#'
#' @return a list of model flows.

get.silly.model.chunk = function(model_flows, init.cond, init.cond.numeric.vars, which.flow, NewFrom, NewTo)
{
  #browser()
  inflow.orphans = outflow.orphans = c()
  silly_model = model_flows
  if(which.flow %in% c("inflow","inflows"))
  {
    silly_model$To   = paste0(NewTo,silly_model$To)
    silly_model$From = NewFrom
    init.prefix      = NewTo
    init.slack.name  = NewFrom
    inflow.orphans   = setdiff(init.cond$NAME,unique(model_flows$To))
  }
  if(which.flow %in% c("outflow","outflows"))
  {
    silly_model$From = paste0(NewFrom,silly_model$From)
    silly_model$To   = NewTo
    init.prefix      = NewFrom
    init.slack.name  = NewTo
    outflow.orphans  = setdiff(init.cond$NAME,unique(model_flows$From))
  }

  silly_init  = init.cond
  silly_init$NAME  = paste0(init.prefix,silly_init$NAME)
  silly_init[,init.cond.numeric.vars] = 0
  init.slack = silly_init[1,]
  init.slack$NAME = init.slack.name
  silly_init = rbind(silly_init,init.slack)

  #BEGIN handle the orphans
  n.age.grp = length(init.cond.numeric.vars)
  zeros.char = paste0("c(",paste(rep("0",n.age.grp),collapse=","),")")
  model.inflow.orphans = model.outflow.orphans = c()
  if(length(inflow.orphans) > 0)
  {
    model.inflow.orphans = silly_model[rep(1,length(inflow.orphans)),]
    model.inflow.orphans$To = paste0(NewTo,inflow.orphans)
    model.inflow.orphans$expression = zeros.char
  }
  if(length(outflow.orphans) > 0)
  {
    model.outflow.orphans = silly_model[rep(1,length(outflow.orphans)),]
    model.outflow.orphans$From = paste0(NewFrom,outflow.orphans)
    model.outflow.orphans$expression = zeros.char
  }
  #END handle the orphans

  list(init=silly_init , model = rbind(silly_model,model.inflow.orphans,model.outflow.orphans) )
}


#' Save box/compartment model as an Excel workbook
#'
#' @export
#'
#' @importFrom openxlsx write.xlsx
#'
#' @param input.info.list a list.
#' @param file_name a character element.
#' @param map.names a list of workbook sheet names.
#'
#' @return none.

save.model.in.workbook = function(input.info.list,file_name,map.names)
{  # map.names is named vector saying map.names["parms.1d"  ] =  "Parameters by Age"
  # map.names = unlist(sheet_names) # allows to map "parms.1d" into "Parameters by Age" for instance
  names(input.info.list) = map.names[names(input.info.list)] # $parms.1d is now $'Parameters by Age'
  openxlsx::write.xlsx(input.info.list, file_name, colWidths = c(NA, "auto", "auto"))
}

#' Perform a parameter sweep
#'
#' @export
#'
#' @importFrom lhs randomLHS
#' @importFrom stats qunif runif
#' @importFrom triangle qtriangle
#'
#' @param seir.object a box/compartment model object.
#' @param parm.cloud.grid.specs a data frame of parameter sweep specifications.
#' @param only.show.parms.to.try a logical value. By default, FALSE.
#'
#' @return a list of parameter sweep inputs and results.

try.various.parms.values = function(seir.object,parm.cloud.grid.specs,only.show.parms.to.try=FALSE)
{
  #parm.cloud.grid.specs is a list that should contain the following 7 things
  # *  $hypercube.lower.bounds , $hypercube.upper.bounds, $hypercube.apex.mode
  # *  $n.repeat.within.hypercube
  # *  $LatinHypercubeSampling
  # *  $racine
  # *  $tmin.alter.scope
  # *  $backend.transformation
  # *  $reference.alteration

  lower.bound      = parm.cloud.grid.specs$hypercube.lower.bounds
  upper.bound      = parm.cloud.grid.specs$hypercube.upper.bounds
  apex             = parm.cloud.grid.specs$hypercube.apex.mode  # may be NULL
  n.repeat         = parm.cloud.grid.specs$n.repeat.within.hypercube
  tmin.alter.scope = parm.cloud.grid.specs$tmin.alter.scope
  # racine           = parm.cloud.grid.specs$racine
  # backend.transformation = parm.cloud.grid.specs$backend.transformation
  # reference.alteration      = parm.cloud.grid.specs$reference.alteration

  set.seed(parm.cloud.grid.specs$racine)

  operation_list = list(overwrite = function(current,new)         {0*current+new        } ,
                        add       = function(current,increment  ) {  current+increment  } ,
                        multiply  = function(current,mult_factor) {  current*mult_factor} )
  operation_func = operation_list[[parm.cloud.grid.specs$reference.alteration]]
  operation.label = c(overwrite=".overwrite",add=".add",multiply=".multiplier")[parm.cloud.grid.specs$reference.alteration]


  # Compute all possible candidate by multiplier combinations
  #multiplier_combos <- expand.grid(candidates, KEEP.OUT.ATTRS = FALSE)
  lower.bound.expanded = expand.grid(lower.bound, KEEP.OUT.ATTRS = FALSE)
  upper.bound.expanded = expand.grid(upper.bound, KEEP.OUT.ATTRS = FALSE)
  if(any(dim(lower.bound.expanded) != dim(upper.bound.expanded) ))
    stop('dimension mismatch')
  if(any(colnames(lower.bound.expanded) != colnames(upper.bound.expanded) ))
    stop('column name mismatch')

  lower.bound.expanded = lower.bound.expanded[rep(seq(nrow(lower.bound.expanded)) , n.repeat ),,drop=F]
  upper.bound.expanded = upper.bound.expanded[rep(seq(nrow(upper.bound.expanded)) , n.repeat ),,drop=F]
  lower.bound.expanded = data.matrix(lower.bound.expanded)
  upper.bound.expanded = data.matrix(upper.bound.expanded)
  if(!is.null(apex))
  {
    apex.expanded = expand.grid(apex, KEEP.OUT.ATTRS = FALSE)
    apex.expanded = apex.expanded[rep(seq(nrow(apex.expanded)) , n.repeat ),]
    apex.expanded = data.matrix(apex.expanded)
  }

  if(parm.cloud.grid.specs$LatinHypercubeSampling)
    random.vect= c( lhs::randomLHS( nrow(upper.bound.expanded),ncol(upper.bound.expanded) ) )
  else
    random.vect= runif(prod(dim(upper.bound.expanded)))

  # parms.to.try = (upper.bound.expanded - lower.bound.expanded) *  random.vect
  # parms.to.try = lower.bound.expanded + parms.to.try
  if(is.null(apex))
    parms.to.try =    stats::qunif    (random.vect,lower.bound.expanded,upper.bound.expanded)
  else
    parms.to.try = triangle::qtriangle(random.vect,lower.bound.expanded,upper.bound.expanded,apex.expanded)

  parms.to.try = 0*lower.bound.expanded + parms.to.try
  parms.to.try = parm.cloud.grid.specs$backend.transformation(parms.to.try)

  parms.to.try.verbose = as.data.frame(parms.to.try)
  colnames(parms.to.try.verbose) = paste0(colnames(parms.to.try),operation.label)
  rownames(parms.to.try.verbose) = c()

  if(only.show.parms.to.try)
    return (list(parms.to.try=parms.to.try.verbose))


  # Recover some info from baseline/template (i.e. SEIR.object)

  file_name_for_sweep   = seir.object$input.info.verbatim$file.name
  sheet_names_for_sweep = seir.object$input.info           # Can use either of those two lines ... in theory (not tested)
  sheet_names_for_sweep = seir.object$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)

  baseline.parms.notime.0d = seir.object$input.info$parms.notime.0d  # data frame of 0d parameters (i.e. not by age)
  baseline.parms.0d = seir.object$input.info$parms.0d  # data frame of 0d parameters (i.e. not by age)
  baseline.parms.1d = seir.object$input.info$parms.1d  # data frame of 1d parameters (i.e. by age)
  baseline.parms.2d = seir.object$input.info$parms.2d  # data frame of 2d parameters (i.e. by age and age)

  functions.kit     = seir.object$functions
  # post.process.kit  = SEIR.object$functions$post.processing.companion.kit
  agegrp.glue       = seir.object$input.info.verbatim$agegrp.glue
  CTMC.racines.alea = seir.object$input.info.verbatim$CTMC.random.seeds

  flows.of.interest = gsub("solution.","",intersect(names(seir.object),c("solution.inflows","solution.outflows")))

  list.sweep = list() #        store results in list  ...
  df.sweep = c()      # ... or store results in data.frame
  outcomes.summary.df = c()

  for(i in 1:nrow(lower.bound.expanded)) {
    row <-  parms.to.try[i,]
    names(row) = colnames(parms.to.try)
    #this.label <- paste0(names(row),       ".multiplier= ", row, collapse = " , ")
    this.label <- paste0(names(row), operation.label, "= ", row, collapse = " , ")


    cat("\n Doing",i,"of",nrow(parms.to.try),"simulations :",this.label)

    parms.notime.0d = baseline.parms.notime.0d  # data frame of 0d parameters to be altered
    parms.0d = baseline.parms.0d  # data frame of 0d parameters to be altered
    parms.1d = baseline.parms.1d  # data frame of 1d parameters to be altered
    parms.2d = baseline.parms.2d  # data frame of 2d parameters to be altered

    # Modify the parameter values at the specified tmin.alter.scope
    for(parameter in names(row)) {
      if(parameter %in% names(parms.notime.0d))
        parms.notime.0d[1                           , parameter] = operation_func( parms.notime.0d[1,                          parameter]  , row[[parameter]] )
      else if(parameter %in% names(parms.0d))
        parms.0d[parms.0d$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.0d,tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
      else if(parameter %in% names(parms.1d))
        parms.1d[parms.1d$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.1d,tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
      else if(parameter %in% names(parms.2d))
        parms.2d[parms.2d$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.2d,tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
      else
        stop(paste(parameter,"was not found in any parameter sheet"))# Parameter was not found in any parameter sheet

    }

    sheet_names_for_sweep$parms.notime.0d = parms.notime.0d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.0d = parms.0d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.1d = parms.1d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.2d = parms.2d # altered data.frame goes in sheet_names_for_sweep

    this.result = seir.n.age.classes(file_name_for_sweep,sheet_names_for_sweep,also.get.flows=flows.of.interest,functions.kit = functions.kit, agegrp.glue=agegrp.glue, CTMC.random.seeds=CTMC.racines.alea)

    # Add on univariate stuff like maxI or maxI.time to outcomes.summary.df
    summary.template = data.frame(etiquette = this.label)
    for(parameter in names(row))
      summary.template[[paste0(parameter, operation.label)]] <- row[[parameter]]

    summary.chunk = this.result$sommaire
    summary.chunk = cbind(summary.template,summary.chunk)
    # summary.chunk$etiquette = this.label # not sure if this is useful to keep
    # for(parameter in names(row))
    #  summary.chunk[[paste0(parameter, operation.label)]] <- row[[parameter]]

    outcomes.summary.df = rbind(outcomes.summary.df,summary.chunk)
    #print(names(outcomes.summary.df))

    # Add this.label and the parameter multpliers to the this.result$solution data frame
    #this.result$solution$etiquette <- this.label
    for(parameter in names(row))
      this.result$solution[[paste0(parameter, operation.label)]] <- row[[parameter]]

    #Update list.sweep and df.sweep
    list.sweep[[this.label]] = this.result[c("solution","input.info","input.info.verbatim")] # this.result$solution
    df.sweep = rbind(df.sweep,this.result$solution)
  }
  rownames(outcomes.summary.df) = c()
  outcomes.summary.df$etiquette = c() # drop etiquette.  Not really useful to keep.

  list(parm.cloud.grid.specs = parm.cloud.grid.specs,
       parms.to.try = parms.to.try.verbose,
       outcomes.summary.df = outcomes.summary.df,
       df.sweep = df.sweep,
       list.sweep=list.sweep)
}

#' Assess parameter importance
#'
#' @export
#'
#' @importFrom stats cor lm
#'
#' @param don a data frame.
#' @param X a numeric vector.
#' @param Y a numeric vector.
#' @param method a character element ("kendall-partial-correlation-slow", "pearson-partial-correlation-fast", #' "pearson-partial-correlation-slow", "spearman-partial-correlation-slow", "negative-log-p-value", "t-test").
#'
#' @return a vector.

assess.parameter.importance = function (don,X,Y,method)
{
  #  if(method == "ANOVA SS type ???")
  #  if(method == "Stepwise")
  #  if(method == "Regression tree")
  #  if(method == "Et cetera...")

  if(method %in% c( "negative-log-p-value","t-test","pearson-partial-correlation-fast") )
  {
    formule = eval(parse(text= paste(Y,"~",paste(X,collapse="+")) ))
    reg = summary(lm(formule,data=don))
    if(method =="negative-log-p-value" )
      result = -log(reg$coefficients[-1,"Pr(>|t|)"] )
    else
    { # "t-test" or "pearson-partial-correlation-fast"
      result = reg$coefficients[-1,"t value"] # result for "t-test" ... "PPCF" needs more work below
      if(method == "pearson-partial-correlation-fast")
        result = result / sqrt(reg$df[2] + result**2) #  "pearson-partial-correlation-fast" aka "PPCF"
    }
  }

  if(grepl("-partial-correlation-slow",method ) )
  {
    cor.method = sub("-partial-correlation-slow","",method)
    result = c()
    for(this.X in X)
    {
      other.covariates = paste(setdiff(X,this.X),collapse="+")
      formule.Y      = eval(parse(text= paste(Y     ,"~",other.covariates) ))
      formule.this.X = eval(parse(text= paste(this.X,"~",other.covariates) ))
      result = c(result,cor(lm(formule.Y,data=don)$residuals,lm(formule.this.X,data=don)$residuals,method=cor.method))
    }
    names(result) = X
  }

  result
}

#' Compare results from box/compartment models
#'
#' @export
#'
#' @param solution1 a box/compartment model object.
#' @param solution2 a box/compartment model object.
#' @param age.suffix2 a character element.
#' @param ignore.vars a logical element.
#' @param time.scope a numeric vector.
#' @param tolerance a list with absolute and relative variables.
#'
#' @return a vector.

compare.models = function(solution1,solution2,age.suffix2="",ignore.vars=NULL,time.scope=c(0,Inf),tolerance=list(absolute=2,relative=1e-4))
{
  if(is.list(solution1) && !is.data.frame(solution1) && "solution" %in% names(solution1))
    solution1 = solution1$solution
  if(is.list(solution2) && !is.data.frame(solution2) && "solution" %in% names(solution2))
    solution2 = solution2$solution


  if(nrow(solution1) != nrow(solution2))
    stop("\n Each $solution should have the same number of rows.")
  # if(any(solution1$time != solution2$time))
  #   stop("\n Each $solution should have the same times")

  time.subset   = time.scope[1] <=  solution1$time  & solution1$time <= time.scope[2]

  vars.actually.done = mat.diff = c()
  vars.to.check = setdiff(colnames(solution1), c("time",ignore.vars) )
  for(this.box in vars.to.check )
  {
    names2 = paste0(this.box,age.suffix2)
    if(all(names2 %in% names(solution2)))
    {
      #somme = solution2[,this.box]
      somme = apply(solution2[,paste0(this.box,age.suffix2),drop=FALSE],1,sum)
      actual.diff = abs(solution1[,this.box] - somme)[time.subset]
      rel.diff = actual.diff /( 1e-9 +  pmax(abs(solution1[,this.box]) , abs(somme)) )[time.subset]
      bad.counts = sum( (abs(actual.diff)>tolerance$absolute & abs(rel.diff) > tolerance$relative) )
      # this.box.diff = c(range(rel.diff[time.subset] ) , range(actual.diff[time.subset]),bad.counts)
      this.box.diff = c(max(rel.diff ) , max(actual.diff),bad.counts)
      mat.diff = rbind(mat.diff,this.box.diff)
      vars.actually.done = c(vars.actually.done,this.box)
      #cat("\n",this.box,"\t", this.box.diff)
    }
  }
  rownames(mat.diff) = vars.actually.done
  mat.diff=cbind(mat.diff,seq(nrow(mat.diff)))
  #colnames(mat.diff) = c("rel.diff.min","rel.diff.max","diff.min","diff.max","bad.both","var.num")
  colnames(mat.diff) = c(               "rel.diff.max",           "diff.max","bad.both","var.num")
  as.data.frame(mat.diff)
}

#' Save R objects to a file
#'
#' @description
#' This function is a wrapper for the base `R` [save()] function that adds prefix/suffix information and compression to the R objects that are saved to file.
#'
#' @export
#'
#' @param object.name a character element representing the R object to be saved.
#' @param prefix.suffix a named (prefix, suffix), two-element character vector representing the prefix and suffix for the file name. By default, the prefix is "This file contains an R object called " and the suffix is ".SavedFromR".
#' @param path.with.trailing.slash set to null by default.
#' @param time.stamp the format for the timestamp that appears in the file name. By default, the format is yyyy-mm-dd hh-mm-ss.
#'
#' @return none.

verbose.save = function(object.name,path.with.trailing.slash="",prefix.suffix=c(prefix="This file contains an R object called ",suffix=".SavedFromR"),time.stamp=gsub(":","-",Sys.time()))
{
  if(time.stamp != "")
    time.stamp = paste0(" (", time.stamp,")")

  code = paste0(prefix.suffix["prefix"],object.name,time.stamp,prefix.suffix["suffix"])
  code = paste0("save(",object.name,",file='",path.with.trailing.slash,code,"', compress = 'xz')")
  eval(parse(text=code))
}

#' Get hypercube sampling specifications
#'
#' @export
#'
#' @importFrom readxl read_excel
#'
#' @param file.name the name of the Excel workbook where the specifications are located.
#' @param sheet the name of the sheet where the specifications are located.
#'
#' @return a list.

read.hypercube.sampling.specs = function(file.name,sheet)
{
  look = as.data.frame ( readxl::read_excel(file.name, sheet = sheet) )
  #look =                 readxl::read_excel(file.name, sheet = sheet)
  if( any( range(look$hypercube )!= 1 ) )
    stop("\n Can only do 1 hypercube for now")

  liste=list()
  for(k in c("lower.bound",    "upper.bound",    "apex" ))
  {
    tempo=as.numeric(look[,k])
    names(tempo)=look$parameter.name
    liste[[k]]  = as.list(tempo)
  }
  liste
}
