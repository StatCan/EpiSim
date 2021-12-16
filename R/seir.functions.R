#' Main box/compartment model functions
#'
#' @export
#'
#' @importFrom adaptivetau ssa.adaptivetau
#' @importFrom deSolve lsoda ode
#' @importFrom dplyr arrange filter is.tbl mutate select slice
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#'
#' @param file.name a character element.
#' @param sheets.names a list.
#' @param episim.controls a list containing one or more of the following: $run.until, $insert.tmins, $insert.tmin.char, $times.of.interest, $import.system.snapshots, $export.system.snapshot.time, $deSolve.controls (by default, NULL).
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

seir.n.age.classes = function(file.name, sheets.names, episim.controls=NULL,  functions.kit=NULL,
                              also.get.flows=NULL, agegrp.glue="", CTMC.random.seeds=NULL) # agegrp.glue=".ag"
{
  # functions.kit may be NULL or a list containing SOME of the following
  #   $differential.eqns.func
  #   $post.processing.func
  #   $post.processing.companion.kit
  #   $CTMC.eqns.func
  #   $CTMC.eqns.func.companion


  time.stamps = data.frame(begin=Sys.time())

  times.of.interest = episim.controls$times.of.interest


  if(is.null(functions.kit$CTMC.eqns.func.companion) != is.null(functions.kit$CTMC.eqns.func))
    stop("\n CTMC.eqns.func and CTMC.eqns.func.companion must be both provided or both omitted")

  expected.sheets.names = c("parms.notime.0d","parms.0d","parms.1d","parms.2d","initial.conditions","model.flow","auxiliary.vars","post.processing")

  if( !all( expected.sheets.names %in% names(sheets.names) ) )
    stop( paste("\n sheets.names must provide the following:",paste(expected.sheets.names,collapse=", ")) )

  auxiliary.calculations.kit = sheets.names [ setdiff(names(sheets.names),expected.sheets.names)] # sheets.names with expected.sheets.names removed (may be empty list)



  stop.if.tbl = function(x)
  {
    if(dplyr::is.tbl(x))
      stop (paste("\n tbl object detected:" , deparse(substitute(x)) ))
  }


  input.info.verbatim = sheets.names
  input.info.verbatim$file.name = file.name
  input.info.verbatim$agegrp.glue = agegrp.glue
  input.info.verbatim$CTMC.random.seeds = CTMC.random.seeds
  #==========================================================================
  #   Initial conditions and parameters
  #==========================================================================

  read.parms = function(file.name,char.or.list)
  {
    if(is.list(char.or.list))
      return (char.or.list)  # char.or.list is a list or a data.frame (initial.conditions) --> return as is

    char.or.list = char.or.list[!is.na(char.or.list)]  # remove NA
    list.parms = list()
    for(k in char.or.list)
      list.parms[[                k ]] = as.data.frame ( readxl::read_excel(file.name, sheet = k) )
    # list.parms[[paste("Source:",k)]] = as.data.frame ( readxl::read_excel(file.name, sheet = k) ) # not helpful if going to use save.model.in.workbook

    list.parms
  }

  substitute.character = function(dframe.to.alter, df.vars.to.replace, replace.from.to, move.to.end)
  { # replaces char.to.replace = "?" (say) by replace.with = "Elvis1", "Elvis2" "Elvis3"  in
    # So this dframe
    #       var1   var2   var3 var4
    #       Salut   44     ABC   55
    # Hello?There    0     A?B    2
    #     Goodbye    9     DEF    0
    #    Hi?There    1     GHI   44
    # becomes this (move.to.end = FALSE)
    #             var1   var2       var3 var4
    #            Salut     44        ABC   55
    # HelloElvis1There      0   AElvis1B    2
    # HelloElvis2There      0   AElvis2B    2
    # HelloElvis3There      0   AElvis3B    2
    #          Goodbye      9        DEF    0
    #    HiElvis1There      1        GHI   44
    #    HiElvis2There      1        GHI   44
    #    HiElvis3There      1        GHI   44
    # ... or becomes this (move.to.end = TRUE).  Besides moving to end, replace.with kind of acts as outer loop when move.to.end = TRUE is used
    #             var1   var2       var3 var4
    #            Salut     44        ABC   55
    #          Goodbye      9        DEF    0
    # HelloElvis1There      0   AElvis1B    2
    #    HiElvis1There      1        GHI   44
    # HelloElvis2There      0   AElvis2B    2
    #    HiElvis2There      1        GHI   44
    # HelloElvis3There      0   AElvis3B    2
    #    HiElvis3There      1        GHI   44

    char.to.replace = replace.from.to[ 1,1]
    replace.with    = replace.from.to[-1,1]
    replace.with    = replace.with[!is.na(replace.with)]

    if(grepl("#",char.to.replace) )
      stop("\n Should not use # as character to be substituted.\n")


    dframe = dframe.nocomments = dframe.to.alter
    dframe.nocomments[,df.vars.to.replace]=sapply(dframe.nocomments[,df.vars.to.replace,drop=FALSE],stringr::word,sep="#") # remove part after # ... only used for found.char

    dframe$row.number.128451165655423655656 = paste(seq(nrow(dframe)))
   #dframe$found.char.128451165655423655656 = apply( grepl(char.to.replace , as.matrix(dframe[,df.vars.to.replace,drop=FALSE]) ,fixed=TRUE), 1, any) # does not work
   #found.char = sapply (dframe           [,df.vars.to.replace,drop=FALSE]  ,function(x) {grepl(char.to.replace , x,fixed=TRUE)} )
    found.char = sapply (dframe.nocomments[,df.vars.to.replace,drop=FALSE]  ,function(x) {grepl(char.to.replace , x,fixed=TRUE)} )
    dframe$found.char.128451165655423655656 = apply( found.char, 1, any)

    dframe.donotchange  = subset(dframe,!found.char.128451165655423655656)
    dframe.needtochange = subset(dframe, found.char.128451165655423655656)

    if(nrow(dframe.needtochange) == 0)
      return(dframe.to.alter)  # return unchanged

    #BEGIN carry out the substitution on dframe.needtochange
    # Couple of ideas with loops.  Decided to not use for loops.  Code may have been simpler to read ... or not
    #    for(replace.char in replace.with)
    #      dframe.changed = rbind(dframe.changed, gsub(char.to.replace,char.to.replace,dframe.needtochange,fixed=TRUE)  )

    #    for(k in seq(nrow(dframe.needtochange)))
    #      for(replace.char in replace.with)

    replace.with = unique(replace.with[!is.na(replace.with)])
    replace.with.order = sort( paste0( seq(length(replace.with)) ) ) # "1"  "10" "11" "12" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" ... so code below works OK if length > 9
    names(replace.with.order) = replace.with  # if replace.with = c("D" "A", "B"), this allows use to remember that "A" was second because replace.with.order["A"] = "2"

    dframe.needtochange$char.to.replace.128451165655423655656 = char.to.replace # "?"
    mat.needtochange = as.matrix(dframe.needtochange[,c("row.number.128451165655423655656","char.to.replace.128451165655423655656",df.vars.to.replace)])
    mat.changed =  c(t( sapply(replace.with, function(x) {gsub(char.to.replace,x,mat.needtochange,fixed=TRUE) } ) ))
    mat.changed = array(mat.changed,dim(mat.needtochange)*c(length(replace.with),1))
    colnames(mat.changed) = colnames(mat.needtochange)
    mat.changed[,"char.to.replace.128451165655423655656"] = replace.with.order[mat.changed[,"char.to.replace.128451165655423655656"]]  # "A" becomes "2", "B" becomes "3",


    dframe.needtochange[c(df.vars.to.replace,"char.to.replace.128451165655423655656","found.char.128451165655423655656")] = c() # some cleaning before inner_join
    dframe.needtochange = dplyr::inner_join(dframe.needtochange,as.data.frame(mat.changed),by="row.number.128451165655423655656")
    dframe.needtochange$replace.order.128451165655423655656 = replace.with.order[dframe.needtochange$char.to.replace.128451165655423655656]
    dframe.needtochange$row.number.128451165655423655656 = as.numeric(dframe.needtochange$row.number.128451165655423655656)

    #END carry out the substitution on dframe.needtochange

    #BEGIN re-assemble
    if(move.to.end)
    {
      dframe.needtochange = dplyr::arrange(dframe.needtochange ,   replace.order.128451165655423655656 , row.number.128451165655423655656      )
      dframe.out = rbind( dframe.donotchange[names(dframe.to.alter)], dframe.needtochange[names(dframe.to.alter)] )
    }
    else
    {
      dframe.donotchange$row.number.128451165655423655656 = as.numeric(dframe.donotchange$row.number.128451165655423655656)
      dframe.donotchange$replace.order.128451165655423655656 = "Hello" # does not matter because row.number.128451165655423655656 unique and distinct from dframe.needtochange
      keep.vars = c(names(dframe.to.alter), "replace.order.128451165655423655656" , "row.number.128451165655423655656" )
      dframe.out = rbind( dframe.donotchange[keep.vars], dframe.needtochange[keep.vars] )
      dframe.out = dplyr::arrange(dframe.out,  row.number.128451165655423655656,   replace.order.128451165655423655656 )[names(dframe.to.alter)]
    }

    #END re-assemble

 #   if(sum(df.vars.to.replace=="code") >0)
  #    browser()

    dframe.out

  }# end function substitute.character


  substitute.kit=sheets.names$substitute.kit  # list(replace.this.character,replace.with) # replace.with = df with $initial.conditions, $model.flow

  tmp_parm_notime_0d = read.parms(file.name, sheets.names$parms.notime.0d    )     # 0 dimensional parameters (no age, no time) ... a list
  tmp_parm_0d        = read.parms(file.name, sheets.names$parms.0d           )     # 0 dimensional parameters (no age         ) ... a list
  tmp_parm_1d        = read.parms(file.name, sheets.names$parms.1d           )     # 1 dimensional parameters (   age         ) ... a list
  tmp_parm_2d        = read.parms(file.name, sheets.names$parms.2d           )     # 2 dimensional parameters (   age x age   ) ... a list

  input_stuff        = read.parms(file.name, sheets.names$initial.conditions )     # initial values/conditions                  ... a list or a data.frame
  if(!is.data.frame(input_stuff))
    input_stuff = input_stuff[[1]]

  for(k in names(substitute.kit))
      input_stuff = substitute.character(input_stuff, "NAME", substitute.kit[k], TRUE)

  input_stuff = subset(input_stuff,!duplicated(NAME)) # keeps the first one if there are duplicates

  raw.init.conditions = input_stuff # keep snapshot for output.  input_stuff may be altered later

  if( diff(range(input_stuff$time)) !=0)
    stop("\n 'time' must be provided and should not vary in initial conditions.")

  run.from = input_stuff$time[1]

  run.until = episim.controls$run.until
  run.until = ifelse(is.null(run.until), Inf, run.until) # convert NULL into Inf

  input_stuff = subset(input_stuff,select = -time)

  nagegrp = ncol(input_stuff) - 1            # number of age groups
  init.cond.numeric.vars = setdiff(colnames(input_stuff),"NAME")

  init_list <- list() # Same content as input_stuff but in a list format
  for(k in input_stuff$NAME)
    init_list[[k]] <- as.matrix( subset(input_stuff, NAME == k)[,init.cond.numeric.vars] )



  #BEGIN get raw.compartments.age and pretty.compartments.age
  agegrp.suffix.pretty = agegrp.suffix = ""
  if(nagegrp > 1)
  {
    agegrp.suffix        = 1:nagegrp
    agegrp.suffix.pretty = paste0(agegrp.glue,1:nagegrp) # if agegrp.glue = ".ag" this generates ".ag1" ".ag2" ".ag3"
  }

  raw.compartments =   input_stuff$NAME # e.g.  S D L
  raw.compartments.age = c ( t( outer( raw.compartments,agegrp.suffix       ,paste0) ) ) # e.g. S1    S2    S3    S4    S5    D1    D2    ...
  pretty.compartments.age = c ( t( outer( raw.compartments,agegrp.suffix.pretty,paste0) ) ) # e.g. S.ag1 S.ag2 S.ag3 S.ag4 S.ag5 D.ag1 D.ag2 ...

  if(length(unique(raw.compartments.age)) != length(raw.compartments.age)  )
    stop("\n Ambuiguity from compartment names and ages")
  # Ambiguity could arise if there are 2 compartments named S and S1 (say) and there are > 10 age groups
  # Then S11 could be 11th age group of S or first age group of S1
  #END get raw.compartments.age and pretty.compartments.age

  #BEGIN 1) Sort parameters. 2) Get tmins, max.tmax. 3) Various user error handling
  parm_value_notime_0d = tmp_parm_notime_0d
  parm_value_0d        = tmp_parm_0d
  parm_value_1d        = tmp_parm_1d
  parm_value_2d        = tmp_parm_2d

  all.parm.names = max.tmax = tmins = c()

  for(k in names(parm_value_0d))
  {
    parm_value_0d[[k]] <- dplyr::arrange(parm_value_0d[[k]], tmin        ) # sort by tmin
    tmins    = c(    parm_value_0d[[k]]$tmin  , tmins    )
    max.tmax = c(max(parm_value_0d[[k]]$tmax) , max.tmax )
    all.parm.names = c( names(parm_value_0d[[k]]) , all.parm.names)
    if(max(parm_value_0d[[k]]$tmin) >= max.tmax [1])
      stop(paste("\nMaximum tmin > Maximum tmin in sheet",k,"\n"))
  }

  for(k in names(parm_value_1d))
  {
    parm_value_1d[[k]] <- dplyr::arrange(parm_value_1d[[k]], tmin, agegrp ) # sort by tmin agegrp
    tmins    = c(    parm_value_1d[[k]]$tmin  , tmins    )
    max.tmax = c(max(parm_value_1d[[k]]$tmax) , max.tmax )
    all.parm.names = c( names(parm_value_1d[[k]]) , all.parm.names)
    if(max(parm_value_1d[[k]]$tmin) >= max.tmax [1])
      stop(paste("\nMaximum tmin > Maximum tmin in sheet",k,"\n"))

  }

  for(k in names(parm_value_2d))
  {
    parm_value_2d[[k]] <- dplyr::arrange(parm_value_2d[[k]], tmin, cagegrp, ragegrp ) # sort by tmin cagegrp ragegrp
    tmins    = c(    parm_value_2d[[k]]$tmin  , tmins    )
    max.tmax = c(max(parm_value_2d[[k]]$tmax) , max.tmax )
    all.parm.names = c( names(parm_value_2d[[k]]) , all.parm.names)
    if(max(parm_value_2d[[k]]$tmin) >= max.tmax [1])
      stop(paste("\nMaximum tmin > Maximum tmin in sheet",k,"\n"))

  }

  if(diff(range(max.tmax)) > 0)
    stop("\nMaximum tmax should be the same in all sheets providing parameter values.\n")

  excluded_names <- c("tmin", "tmax","agegrp","cagegrp","ragegrp")
  freq.parm.names = table(setdiff(all.parm.names,excluded_names))
  freq.parm.names = freq.parm.names[freq.parm.names>1]
  if(length( freq.parm.names )>0)
    stop(paste("\n Parameters specified multiple times:" ,names(freq.parm.names),"\n") )

  if(run.until < max.tmax[1])
    tmins = c(tmins,run.until)

  #END 1) Sort parameters. 2) Get tmins, max.tmax. 3) Various user error handling


  get.parameter.chunk = function(liste,insert.this.tmin.tmax)
  {
    tmin.tmax.present = any(names(liste[[1]]) == "tmin")

    parm.chunk = c()
    for(k in names(liste)) # e.g. "Parm by age (part 1)" "Parm by age (part 2)" "Parm by age (part 3)"
    {
      if(tmin.tmax.present)
      {
        relevant.tmin = unique(liste[[k]]$tmin)
        relevant.tmin = max( relevant.tmin [relevant.tmin <= insert.this.tmin.tmax[1]] )
        add.chunk = subset(liste[[k]] , tmin==relevant.tmin)
      }
      else
        add.chunk = liste[[k]]

      if(is.null(parm.chunk))
        parm.chunk = add.chunk
      else
        parm.chunk = cbind(parm.chunk, add.chunk[,setdiff(names(add.chunk),names(parm.chunk) )]  )

    }
    if(tmin.tmax.present)
    {
      parm.chunk$tmin = insert.this.tmin.tmax[1]
      parm.chunk$tmax = insert.this.tmin.tmax[2]
    }

    parm.chunk
  }
 # browser()



  parm_value_notime_0d.df = get.parameter.chunk(parm_value_notime_0d , "insert.this.tmin.tmax not needed" )

  #BEGIN alter some controls based on what was provided in parm_value_notime_0d (alteration of deSolve.controls is done later)
  #if(run.until > 0)
  #  browser()

  insert.extra.tmin = parm_value_notime_0d.df[episim.controls$insert.tmin.char]
  if(ncol(insert.extra.tmin)==1) # found variable with proper name  ( episim.controls$insert.tmin.char )
    episim.controls$insert.tmins = c(episim.controls$insert.tmins , insert.extra.tmin[,1])

  #END alter some controls based on what was provided in parm_value_notime_0d (alteration of deSolve.controls is done later)

# CTMC.parms.info = list(tmin.vect=sort( unique (c(tmins,run.from,episim.controls$insert.tmins) )) ,parms.0d=list(),parms.1d=list(),parms.2d=list() )
  CTMC.parms.info = list(tmin.vect=sort( unique (c(tmins,    0   ,episim.controls$insert.tmins) )) ,parms.0d=list(),parms.1d=list(),parms.2d=list() )
  CTMC.parms.info$tmin.vect = CTMC.parms.info$tmin.vect[CTMC.parms.info$tmin.vect< max.tmax[1]] # $tmin.vect should not be contaminated with large $insert.tmins
  nTimeSegments <- length(CTMC.parms.info$tmin.vect)


 # overlap.length = function(L1,U1,L2,U2) { pmax(pmin(U1,U2) - pmax(L1,L2),0) } # computes length of intersection of (L1,U1) and (L2,U2) # part of EpiSim since version 0.12.15


  segments.labels = c()
  for(segment in seq(1, nTimeSegments, 1))
  {
    insert.this.tmin.tmax = c(CTMC.parms.info$tmin.vect,max.tmax[1])[segment+(0:1)]
   #if(overlap.length(run.from,run.until,insert.this.tmin.tmax[1],insert.this.tmin.tmax[2]))
    if(overlap.length(0       ,run.until,insert.this.tmin.tmax[1],insert.this.tmin.tmax[2])) # do as if running from time 0 for now
    {
      CTMC.parms.info$parms.0d[[segment]] = get.parameter.chunk(parm_value_0d , insert.this.tmin.tmax ) # this is a data.frame
      CTMC.parms.info$parms.1d[[segment]] = get.parameter.chunk(parm_value_1d , insert.this.tmin.tmax ) # this is a data.frame
      CTMC.parms.info$parms.2d[[segment]] = get.parameter.chunk(parm_value_2d , insert.this.tmin.tmax ) # this is a data.frame

      segments.labels = c(segments.labels,paste(  insert.this.tmin.tmax ,collapse = "-->") )

      CTMC.parms.info$parms.0d[[segment]] = cbind(CTMC.parms.info$parms.0d[[segment]] , parm_value_notime_0d.df)

      # Right now CTMC.parms.info$parms.2d[[segment]] is data.frame (akin to CTMC.parms.info$parms.0d[[segment]] and CTMC.parms.info$parms.1d[[segment]] )
      # Consider Changing CTMC.parms.info$parms.2d[[segment]] from data.frame to list of matrices
      # look for temp[cbind(age.age.parms$ragegrp, age.age.parms$cagegrp)]
    }

  }

  if(length(segments.labels) > 0)
  {
    names(CTMC.parms.info$parms.0d) = segments.labels
    names(CTMC.parms.info$parms.1d) = segments.labels
    names(CTMC.parms.info$parms.2d) = segments.labels
  }


  rm(parm_value_notime_0d, parm_value_0d, parm_value_1d, parm_value_2d, segments.labels, parm_value_notime_0d.df, nTimeSegments)

  #===================================================================
  # Build function eval.post.processing.func (if not provided)
  #===================================================================

  code.body.df = sheets.names$post.processing # data.frame or string
  if(!is.data.frame(code.body.df))
    code.body.df = as.data.frame ( readxl::read_excel(file.name, sheet = code.body.df[1] ) )

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
                       "companion.kit       = list.solution.etcetera$functions$post.processing.companion.kit",
                       "input.info.verbatim = list.solution.etcetera$input.info.verbatim",
                       "initial.conditions  = list.solution.etcetera$input.info$initial.conditions",
                       "initial.conditions$time  = c() # There was no time before EpiSim 0.12.15",
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
    auxiliary.vars   <- as.data.frame ( readxl::read_excel(file.name, sheet = auxiliary.vars[1] ) )  # auxiliary variables in equations

  stop.if.tbl(auxiliary.vars)
  auxiliary.vars = subset(auxiliary.vars, !is.na(code)   )  # auxiliary.vars$code will be top portion of with(...,{ ... })  part
  for(k in names(substitute.kit))
      auxiliary.vars = substitute.character(auxiliary.vars, "code", substitute.kit[k], FALSE)


  if(!is.data.frame(model_flows_tmp))
    model_flows_tmp  <- as.data.frame ( readxl::read_excel(file.name, sheet = model_flows_tmp[1]) )  # arrows in flowchart

  stop.if.tbl(model_flows_tmp)
  model_flows_tmp = subset( model_flows_tmp,!is.na(expression) & activation ==1)
  for(k in names(substitute.kit))
      model_flows_tmp = substitute.character(model_flows_tmp, c("From","To","expression"), substitute.kit[k], TRUE)
 # browser()
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
      dplyr::mutate(fromstar = ifelse(is.na(multiplier),"",paste0(From,"*(")) ,
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

    ODE.func.header.char = c("function(list.boxes, list.parms, auxiliary.calculations.kit, time.now, flow.multiplier=list(inflow=1,outflow=-1)){" ,"#browser() #not a good place for it")
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

    spy.ODE=functions.kit$spy.ODE
   #ODE.within.with.part.char = paste(c(              differential.eqns.char,"#browser()" ,out.char,"#browser()" ,names.out.char,"#print(sum(out));#print(cbind(out));#browser()","list(out)"),collapse="\n") # First tried ";\n" but realised that "\n" is sufficient
    ODE.within.with.part.char = paste(c(spy.ODE$spot3,differential.eqns.char,spy.ODE$spot4,out.char,spy.ODE$spot5,names.out.char,spy.ODE$spot6                                   ,"list(out)"),collapse="\n") # First tried ";\n" but realised that "\n" is sufficient


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
    tmp.eval.differential.eqns.func = paste(c( ODE.func.header.char, spy.ODE$spot1,  ODE.with.char, spy.ODE$spot2, auxiliary.vars$code,  ODE.within.with.part.char,"})" ,"}"),collapse="\n" ) # code of eval.differential.eqns.func
    tmp.ratefunc.for.adaptivetau    = paste(c(CTMC.func.header.char, spy.ODE$spot1, CTMC.with.char, spy.ODE$spot2, auxiliary.vars$code, CTMC.within.with.part.char,"})" ,"}"),collapse="\n" ) # code of ratefunc.for.adaptivetau

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

  resultat = list(input.info.verbatim = input.info.verbatim)

  resultat$input.info = list(parms.notime.0d=tmp_parm_notime_0d , parms.0d=tmp_parm_0d, parms.1d=tmp_parm_1d, parms.2d=tmp_parm_2d, initial.conditions=raw.init.conditions,
                             auxiliary.vars=auxiliary.vars, model.flow=model_flows_tmp, post.processing=code.body.df) # original 8

  resultat$input.info$episim.controls = episim.controls

  resultat$input.info[names(auxiliary.calculations.kit)]  = auxiliary.calculations.kit # append auxiliary.calculations.kit  to resultat$input.info  (former may be empty list)

  resultat$functions = list( differential.eqns.func    = eval.differential.eqns.func ,
                             CTMC.eqns.func            =    ratefunc.for.adaptivetau,
                             CTMC.eqns.func.companion  = transitions.for.adaptivetau,
                             post.processing.func      = eval.post.processing.func,
                             post.processing.companion.kit = post.processing.companion.kit )


  if(run.until <=0)
    return ( resultat )

  # Should do some checks about run.from, run.until, max.tmax[1].  What follows may not be adequate and/or sufficient.
  if(run.from >= run.until)
    stop("\n Unexpected event: run.from >= run.until \n")

  #BEGIN build deSolve.controls
  deSolve.controls = list(method="lsoda", rtol=1e-6, atol=1e-6,  verbose = FALSE, maxsteps = 5000)
# deSolve.controls = c(deSolve.controls , list(maxordn = 12, maxords = 5,jacfunc = NULL, jactype = "fullint")) # These 4 options are now without default values.  User can still use those options by providing them.
# deSolve.controls$Elvis = 44 # only for testing purposes

  if(is.character(episim.controls$deSolve.controls)) # FALSE if episim.controls is NULL or episim.controls$deSolve.controls is NULL or not a character
    episim.controls$deSolve.controls =  CTMC.parms.info$parms.0d[[1]][episim.controls$deSolve.controls] # overwrite episim.controls$deSolve.controls based on what was provided in Parameter sheets

  if(!is.null(episim.controls$deSolve.controls))
    names(episim.controls$deSolve.controls) = sub("deSolve.","",names(episim.controls$deSolve.controls))

  if( !is.null(episim.controls$deSolve.controls) && any(duplicated(names(episim.controls$deSolve.controls))) )
    stop("\nDuplicate in deSolve.controls.  They may arise from improper use of nicknames (e.g. both atol and deSolve.atol provided).\n")

  if(is.numeric(episim.controls$deSolve.controls$method))
    episim.controls$deSolve.controls$method = c("lsoda", "lsode", "lsodes","lsodar","vode", "daspk", "euler", "rk4", "ode23", "ode45", "radau")[episim.controls$deSolve.controls$method]

  if(is.list(episim.controls$deSolve.controls)) # FALSE if episim.controls is NULL or episim.controls$deSolve.controls is NULL or not a list
    deSolve.controls[names(episim.controls$deSolve.controls)] = episim.controls$deSolve.controls # overwrite default deSolve.controls.

  #END build deSolve.controls

  #==========================================================================
  #  Main routine
  #==========================================================================
  # The SEIR model with N age classes
  #
  SEIR.n.Age.Classes.within.loop <- function( time=NULL, tmin.vect=NULL, ageless.parms = NULL, age.parms = NULL, age.age.parms = NULL,list.inits = NULL, not.parms=  c("tmin", "tmax", "agegrp", "cagegrp", "ragegrp"),deSolve.call=NULL)
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

      eval.differential.eqns.func(list.inits, list.parms, auxiliary.calculations.kit, time)


    } #end of function calculate_derivatives

    ###---------------------------------------------------------------------------------
    ### Solver for Ordinary Differential Equations (ODE), Switching Automatically
    ### Between Stiff and Non-stiff Methods
    ###---------------------------------------------------------------------------------
    #browser()
    list.parms  = list.parms.for.lsoda
  # output <-  deSolve::lsoda(y = unlist(list.inits),
  #                            times = time,
  #                            func =  calculate_derivatives,
  #                            parms = list.parms,
  #                            names.inits = names(list.inits))

  #     output <- deSolve::ode(  y           = unlist(list.inits),
  #                              times       = time,
  #                              func        = calculate_derivatives,
  #                              parms       = list.parms,
  #                              names.inits = names(list.inits),
  #                              method  = deSolve.controls$method,
  #                              rtol    = deSolve.controls$rtol,    atol    = deSolve.controls$atol,
  #                              jacfunc = deSolve.controls$jacfunc, jactype = deSolve.controls$jactype,
  #                              maxordn = deSolve.controls$maxordn, maxords = deSolve.controls$maxords,
  #                              maxsteps= deSolve.controls$maxsteps,
  #                              verbose = deSolve.controls$verbose )


    if(is.null(deSolve.call))
    { # first crack at building deSolve.call.  May potentially have to many control options.  If so, this will be fixed in while loop below.
      deSolve.call = c("y           = unlist(list.inits)",
                       "times       = time",
                       "func        = calculate_derivatives",
                       "parms       = list.parms",
                       "names.inits = names(list.inits)")

      for(k in names(deSolve.controls))
        deSolve.call = c(deSolve.call, paste0(k," = deSolve.controls$",k)) # add on stuff like "rtol    = deSolve.controls$rtol"
    }
   #else do nothing as deSolve.call was figured out earlier and things should work smoothly and silently in the while loop below

    try.again = TRUE
    attemps = 0

    while(try.again)
    {
     #eval(parse(text= paste("output <- try( deSolve::ode( " , paste(deSolve.call,collapse=" , ") , "),silence=TRUE)"  )   )) # somehow silence=TRUE does not work
      eval(parse(text= paste("output <- try( deSolve::ode( " , paste(deSolve.call,collapse=" , ") , "))"               )   )) # Could utils::capture.output help?  Did not try.

      attemps = attemps +1
      try.again=FALSE

      if(is.character(output) && attemps == 1) # try to recover
      {
        output = gsub("=",",",gsub(")",",",output)) # replace "=" and ")" by ","
        output = split_str( output, "," )
        output = grep("deSolve.controls$",output,value=TRUE,fixed=TRUE)
        if(length(output)>0) # found problematic arguments
        {
          for(problematic.arg in output)
            deSolve.call = deSolve.call[!grepl(problematic.arg,deSolve.call,fixed=TRUE)]

          cat("\n Ignore message about following unused argument(s). EpiSim recovered and tried again: ", output)
          cat("\n Ignore message about following unused argument(s). EpiSim recovered and tried again: ", output, "\n")
          try.again = TRUE
        }
      } # end  try to recover
    } # end while


    return(list(deSolve.result=output , deSolve.call= deSolve.call))
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


  previous.tmax <- run.from # CTMC.parms.info$parms.0d[[1]]$tmin # 0 unless initial conditions are not for time 0
  df.out = c()
  updated_init_list = init_list

  system.snapshots.list = NULL
  deSolve.call = NULL  # character string of the call.  Needs to be determined the first time around to figure out which controls to use/dismiss
  for(segment in seq( length(CTMC.parms.info$parms.0d) ))
  {

    this.parameter.by.nothing <- CTMC.parms.info$parms.0d[[segment]]
    this.parameter.by.age     <- CTMC.parms.info$parms.1d[[segment]]
    this.parameter.by.age.age <- CTMC.parms.info$parms.2d[[segment]]

    tmin <- unique(c(this.parameter.by.nothing$tmin, this.parameter.by.age$tmin, this.parameter.by.age.age$tmin))
    tmax <- unique(c(this.parameter.by.nothing$tmax, this.parameter.by.age$tmax, this.parameter.by.age.age$tmax))

    if(length(tmin)>1 || length(tmax)>1 || tmin>=tmax )
      stop(paste0("Unexpected pattern in tmin, tmax for interval ", segment))

    if(overlap.length(run.from,run.until,tmin,tmax))
    {
      # this.segment.times <- seq(0   , tmax - tmin, by = 1)
      this.segment.times <- seq(tmin, tmax       , by = 1)
      this.segment.times = this.segment.times [this.segment.times >= run.from]
      tmin=this.segment.times[1] # update tmin based on above alteration of this.segment.times
      if(!is.null(times.of.interest))
        this.segment.times = unique(c(tmin,intersect(this.segment.times , times.of.interest),tmax))

      if(tmin != previous.tmax)
        stop("\n Interval lower bound not equal to previous interval upper bound for this segment ",segment," (",tmin,"!=",previous.tmax,")")



      previous.tmax <- tmax
      out <- SEIR.n.Age.Classes.within.loop( time=this.segment.times,
                                             tmin.vect = CTMC.parms.info$tmin.vect,
                                             ageless.parms = this.parameter.by.nothing,
                                             age.parms     = this.parameter.by.age,
                                             age.age.parms = this.parameter.by.age.age,
                                             list.inits    = updated_init_list,
                                             deSolve.call  = deSolve.call )
      deSolve.call = out$deSolve.call
      out = out$deSolve.result

      # cat("\nc(is.null(times.of.interest),tmin,tmax,this.segment.times)= ",c(is.null(times.of.interest)+0,tmin,tmax,this.segment.times),"\n")
      # browser()

      out <- as.data.frame(out)
      # ode/lsoda Output diagnostic #######################
      #diagn <- diagnostics.deSolve(out)

      out$time <- this.segment.times # seq(tmin,tmax,1)
      out_skinny <- out %>%
        dplyr::slice(nrow(out)) %>%    # select last row
        tidyr::pivot_longer(-time)     # fat to skinny
      init <- out_skinny$value
      names(init) <- out_skinny$name

      #   rowns <- names(select(out,-c(time)))
      #   out <- out %>%
      #     mutate(N_tot = rowSums(.[rowns]))  # Total number of individuals

      if(!is.null(episim.controls$export.system.snapshot.time) && is.null(system.snapshots.list))
      {
        skinny.snapshot = subset(out,time %in% episim.controls$export.system.snapshot.time)
        if(nrow(skinny.snapshot) == 1)
        {  # get system.snapshots.list$one.time
          out_skinny <- out %>%
            dplyr::filter(time == episim.controls$export.system.snapshot.time ) %>%    # select last row
            tidyr::pivot_longer(-time) %>%    # fat to skinny
            as.data.frame

          out_skinny$rev.name = sapply(out_skinny$name ,rev_str) # vacc0.S1 ... vacc0.S6 vacc0.D1 ... vacc0.D6-> 1S.0ccav ... 6S.0ccav  1D.0ccav ... 6D.0ccav
          out_skinny$age.char = paste0("age",substr(out_skinny$rev.name,1,1))
          substr(out_skinny$rev.name,1,1) = "@"
          out_skinny$rev.name=sub("@","",out_skinny$rev.name)
          out_skinny$NAME = sapply(out_skinny$rev.name ,rev_str)
          out_skinny$rev.name = c()
          out_skinny$name = c()

          system.snapshot.one.time = as.data.frame( tidyr::pivot_wider(out_skinny, names_from = age.char, values_from = value) )
          names(system.snapshot.one.time)[!(names(system.snapshot.one.time) %in% c("time","NAME"))] = init.cond.numeric.vars
          system.snapshots.list = list(single.time = system.snapshot.one.time)
        }
      }


      #updating the initial values
      for(k in 1:length(updated_init_list)){
        updated_init_list[[k]][1:nagegrp] <- init[seq(nagegrp*(k-1)+1,nagegrp*k)]
      }

      if(segment < length(CTMC.parms.info$parms.0d) )
        out = out[-nrow(out),]
      # Add outputs to the list
      df.out = rbind(df.out,out)
    } # END if(overlap.length(run.from,run.until,tmin,tmax))



  } #end for(segment in ... )

  if(!is.null(episim.controls$import.system.snapshots))
    df.out = rbind( subset(episim.controls$import.system.snapshots,time < min(df.out$time)) , df.out)

  if(!is.null(episim.controls$export.system.snapshot.time) && !is.null(system.snapshots.list))
    system.snapshots.list$all.times = subset(df.out,time <= episim.controls$export.system.snapshot.time)



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

  resultat$system.snapshots = system.snapshots.list
  time.stamps$end = Sys.time()
  resultat$time.stamps = time.stamps


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
{  # map.names is named list saying map.names[["initial.conditions"  ]] =  "Initial conditions"  for instance
   # map.names is only needed for the last 4 pieces: "initial.conditions", "model.flow", "auxiliary.vars", "post.processing"
   # One may/should use  map.names = input.info.verbatim

 # browser()
  expected.pieces = c("parms.notime.0d","parms.0d","parms.1d","parms.2d","initial.conditions","model.flow","auxiliary.vars","post.processing") # same as expected.sheets.names in seir.n.age.classes

  if( !all( expected.pieces %in% names(input.info.list) ) )
    stop( paste("\n input.info.list must provide the following:",paste(expected.pieces,collapse=", ")) )

  output.list = list()
  for(piece in expected.pieces)
  {
    this.chunk = input.info.list[[piece]]
    if(is.data.frame(this.chunk))
      output.list[[ map.names[[piece]][1] ]]= this.chunk # we use map.names[[piece]][1] as map.names[[piece]] will typically be of length > 1 (e.g. c("Something", NA)  of length 2 here)
    else # this.chunk is a list.  Furthermore, it is a named list and all its members are data.frames.
      output.list[ names(this.chunk) ] = this.chunk # add on the content of this.chunk to output.list
  }

  openxlsx::write.xlsx( output.list, file_name, colWidths = c(NA, "auto", "auto"))
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
#' @param dump.progress a list with components $location and $naming.extra. By default, NULL.
#'
#' @return a list of parameter sweep inputs and results.

try.various.parms.values = function(seir.object,parm.cloud.grid.specs,only.show.parms.to.try=FALSE,dump.progress=NULL)
{
  #parm.cloud.grid.specs is a list that should contain the following components
  # *  $backend.transformation
  # *  $reference.alteration
  # *  $tmin.alter.scope
  # along with either component parms.to.try.raw or the components listed below (which allows one to get/compute parms.to.try.raw)
  # *  $hypercube.lower.bounds , $hypercube.upper.bounds, $hypercube.apex.mode
  # *  $n.repeat.within.hypercube
  # *  $LatinHypercubeSampling
  # *  $racine



  if(is.null(parm.cloud.grid.specs$message.length))
    parm.cloud.grid.specs$message.length = 9999

  if(is.null(parm.cloud.grid.specs$parms.to.try.raw))
  { # BEGIN Get parms.to.try.raw from scratch
    # It is here and only here that we use
    # *  $hypercube.lower.bounds , $hypercube.upper.bounds, $hypercube.apex.mode
    # *  $n.repeat.within.hypercube
    # *  $LatinHypercubeSampling
    # *  $racine

    set.seed(parm.cloud.grid.specs$racine)
    lower.bound      = parm.cloud.grid.specs$hypercube.lower.bounds
    upper.bound      = parm.cloud.grid.specs$hypercube.upper.bounds
    apex             = parm.cloud.grid.specs$hypercube.apex.mode  # may be NULL
    n.repeat         = parm.cloud.grid.specs$n.repeat.within.hypercube


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


    if(is.null(apex))
      parms.to.try.raw =    stats::qunif    (random.vect,lower.bound.expanded,upper.bound.expanded)
    else
      parms.to.try.raw = triangle::qtriangle(random.vect,lower.bound.expanded,upper.bound.expanded,apex.expanded)

    parms.to.try.raw = 0*lower.bound.expanded + parms.to.try.raw

    # END Get parms.to.try.raw from scratch

    #parms.to.try.verbose = as.data.frame(parms.to.try)
    #colnames(parms.to.try.verbose) = paste0(colnames(parms.to.try),operation.label)
    #rownames(parms.to.try.verbose) = c()
  }# END is.null(parm.cloud.grid.specs$parms.to.try.raw)
  else
  {  # just use parms.to.try.raw as provided by user
    parms.to.try.raw = as.matrix( parm.cloud.grid.specs$parms.to.try.raw )
  }


  if(only.show.parms.to.try)
  {
     retourner.ceci = parm.cloud.grid.specs
     retourner.ceci$parms.to.try.raw=parms.to.try.raw
     return (retourner.ceci)
  }

  tmin.alter.scope = parm.cloud.grid.specs$tmin.alter.scope

  operation_list = list(overwrite = function(current,new)         {0*current+new        } ,
                        add       = function(current,increment  ) {  current+increment  } ,
                        multiply  = function(current,mult_factor) {  current*mult_factor} )

  operation_func = operation_list[[parm.cloud.grid.specs$reference.alteration]]
  operation.label = c(overwrite=".overwrite",add=".add",multiply=".multiplier")[parm.cloud.grid.specs$reference.alteration]

  parms.to.try = parm.cloud.grid.specs$backend.transformation(parms.to.try.raw)  # e.g. apply exp() function to parms.to.try.raw


  # Recover some info from baseline/template (i.e. SEIR.object)

  file_name_for_sweep   = seir.object$input.info.verbatim$file.name
  sheet_names_for_sweep = seir.object$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)
  sheet_names_for_sweep = seir.object$input.info           # Can use either of those two lines ... in theory (not tested)

  baseline.parms.notime.0d = seir.object$input.info$parms.notime.0d  # data frame of 0d parameters (i.e. not by age)
  baseline.parms.0d = seir.object$input.info$parms.0d  # data frame of 0d parameters (i.e. not by age)
  baseline.parms.1d = seir.object$input.info$parms.1d  # data frame of 1d parameters (i.e. by age)
  baseline.parms.2d = seir.object$input.info$parms.2d  # data frame of 2d parameters (i.e. by age and age)

  functions.kit     = seir.object$functions
  # post.process.kit  = SEIR.object$functions$post.processing.companion.kit
  agegrp.glue       = seir.object$input.info.verbatim$agegrp.glue
  CTMC.racines.alea = seir.object$input.info.verbatim$CTMC.random.seeds

  flows.of.interest = gsub("solution.","",intersect(names(seir.object),c("solution.inflows","solution.outflows")))

  list.input.info     = list()
  list.sweep          = list() #        store results in list  ...
  df.sweep            = c()    # ... or store results in data.frame
  outcomes.summary.df = c()
  time.stamps         = c()

  for(i in 1:nrow(parms.to.try)) {
    row <-  parms.to.try[i,]
    names(row) = colnames(parms.to.try)
    #this.label <- paste0(names(row),       ".multiplier= ", row, collapse = " , ")
    this.label <- paste0(names(row), operation.label, "= ", row, collapse = " , ")

    this.message = paste("\n Doing",i,"of",nrow(parms.to.try),"simulations :",this.label)
    cat(substr(this.message,1,parm.cloud.grid.specs$message.length))

    parms.notime.0d = baseline.parms.notime.0d  # list of data.frames of 0d parameters to be altered
    parms.0d = baseline.parms.0d                # list of data.frames of 0d parameters to be altered
    parms.1d = baseline.parms.1d                # list of data.frames of 1d parameters to be altered
    parms.2d = baseline.parms.2d                # list of data.frames of 2d parameters to be altered

    # Modify the parameter values at the specified tmin.alter.scope
    for(parameter in names(row))
    {
      found=FALSE
      if(!found)
        for(k in names(parms.notime.0d)) # parms.notime.0d is a list, parms.notime.0d[[k]] is a data.frame
          if(parameter %in% names(parms.notime.0d[[k]]))
          {
            parms.notime.0d[[k]][1                           , parameter] = operation_func( parms.notime.0d[[k]][1,                          parameter]  , row[[parameter]] )
            found = TRUE
          }

      if(!found)
        for(k in names(parms.0d)) # parms.0d is a list, parms.0d[[k]] is a data.frame
          if(parameter %in% names(parms.0d[[k]]))
          {
            parms.0d[[k]][parms.0d[[k]]$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.0d[[k]],tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
            found = TRUE
          }

      if(!found)
        for(k in names(parms.1d)) # parms.1d is a list, parms.1d[[k]] is a data.frame
          if(parameter %in% names(parms.1d[[k]]))
          {
            parms.1d[[k]][parms.1d[[k]]$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.1d[[k]],tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
            found = TRUE
          }

      if(!found)
        for(k in names(parms.2d)) # parms.2d is a list, parms.2d[[k]] is a data.frame
          if(parameter %in% names(parms.2d[[k]]))
          {
            parms.2d[[k]][parms.2d[[k]]$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.2d[[k]],tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
            found = TRUE
          }

      if(!found)
        stop(paste(parameter,"was not found in any parameter sheet"))# Parameter was not found in any parameter sheet

    }

    sheet_names_for_sweep$parms.notime.0d = parms.notime.0d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.0d = parms.0d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.1d = parms.1d # altered data.frame goes in sheet_names_for_sweep
    sheet_names_for_sweep$parms.2d = parms.2d # altered data.frame goes in sheet_names_for_sweep

    this.result = seir.n.age.classes(file_name_for_sweep, sheet_names_for_sweep,
                                     episim.controls   = seir.object$input.info$episim.controls,
                                     also.get.flows    = flows.of.interest,
                                     functions.kit     = functions.kit,
                                     agegrp.glue       = agegrp.glue,
                                     CTMC.random.seeds = CTMC.racines.alea)

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

    # Add this.label and the parameter multpliers to the this.result$solution data frame
    #this.result$solution$etiquette <- this.label
    for(parameter in names(row))
      this.result$solution[[paste0(parameter, operation.label)]] <- row[[parameter]]

    #Update  df.sweep and friends
   #list.sweep[[this.label]] = this.result[c("solution","input.info","input.info.verbatim")] # this.result$solution
    list.input.info[[this.label]] = this.result$input.info
    df.sweep    = rbind(df.sweep   ,this.result$solution)
    time.stamps = rbind(time.stamps,this.result$time.stamps)
    results.this.far = list(parm.cloud.grid.specs = parm.cloud.grid.specs,
                            parms.to.try.raw = parms.to.try.raw,
                            time.stamps = time.stamps,
                            outcomes.summary.df = outcomes.summary.df,
                            list.input.info = list.input.info,
                            #list.sweep=list.sweep,
                            df.sweep = df.sweep,
                            system.snapshots=this.result$system.snapshots)

    if(!is.null(dump.progress))
      verbose.save(results.this.far, path.with.trailing.slash = dump.progress$location, time.stamp = dump.progress$naming.extra)

  }  # END for(i in 1:nrow(parms.to.try))

  rownames(results.this.far$outcomes.summary.df) = c()
  results.this.far$outcomes.summary.df$etiquette = c() # drop etiquette.  Not really useful to keep.
  results.this.far

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

  times.common = intersect(solution1$time,solution2$time)
  solution1 = subset(solution1,time %in% times.common)
  solution2 = subset(solution2,time %in% times.common)

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
