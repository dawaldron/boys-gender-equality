library(here)
library(data.table)
library(magrittr)
library(srvyr)
library(haven)

# Get list of folders (results of files downloaded from https://www.icpsr.umich.edu/web/NAHDAP/series/35? and extracted here)
l_dir <- list.dirs(here('data-in','MtF'), recursive = FALSE, full.names = FALSE)

# data file names and paths
dt_data.fn <- lapply(l_dir, function(dir) {
  dir2 <- list.dirs(here('data-in','MtF',dir), recursive = FALSE, full.names = FALSE) %>% .[1]
  l_dir3 <- list.dirs(here('data-in','MtF',dir,dir2), recursive = FALSE, full.names = FALSE)
  dt <- lapply(l_dir3, function(dir3) {
    fn <- list.files(here('data-in','MtF',dir,dir2,dir3))
    fn <- fn[grepl('.dta',fn, fixed = TRUE)]
    dt <- data.table(ds = dir3, fn = fn)
    return(dt)
  }) %>%
    rbindlist()
  dt <- dt %>%
    .[,
      .(dir1 = dir,
        dir2 = dir2,
        ds,
        fn,
        type = ifelse(grepl('.dta',fn,fixed=T), 'dta', 'del'))]
  return(dt)
}) %>%
  rbindlist()

# reference for years
dt_mtf.year <- fread(here('data-in','MtF','icpsr_ref_edit.csv'))

# Item reference numbers
dt_cb.edit <- fread(here('data-in','MtF','irn_ref_edit.csv')) %>%
  .[,
    .(ds, form, varname, irn, vardesc)] %>%
  dt_mtf.year[., on = c('ds','form')] %>%
  .[order(year, ds, form)]

# function to read data based on provided item reference numbers
read_mtf <- function(survey = '12th', irnlist, fn, cb) {
  
  fn <- cb %>%
    .[V4 == survey
      & irn %in% irnlist,
      .(year,
        dir2 = ds,
        ds = paste0('DS000', form),
        varname,
        irn)] %>%
    fn[., on = c('dir2','ds'), nomatch = 0, allow.cartesian = TRUE] %>%
    .[,
      .(dir1,
        dir2,
        ds,
        fn,
        varname,
        year,
        irn)]
  
  fn.file <- fn %>%
    .[!duplicated(cbind(dir1,dir2,ds,fn)),
      .(dir1,dir2,ds,fn,year)]
  
  print(fn)
  
  apply(fn.file, 1, function(x, vars) {
    print(paste0(x[1], '/', x[2], '/', x[3], '/', x[4]))
    sel <- vars %>%
      .[dir1 == x[1] & dir2 == x[2] & ds == x[3] & fn == x[4]] %>%
      .[!duplicated(varname)]
    selvars <- sel[, varname]
    selirns <- sel[, irn]
    
    if (length(selvars) > 0) {
      varlist <- c('V3','V5','ARCHIVE_WT','TABLET', toupper(selvars))
      dt <- read_dta(here('data-in','MtF',x[1],x[2],x[3],x[4]),
                     col_select = any_of(varlist)) %>%
        data.table() %>%
        .[,
          `:=`(ds = x[2],
               form = x[3],
               year = x[5])]
      
      setnames(dt,
               c('RESPONDENT_ID', 'ARCHIVE_WT','V6',toupper(selvars)),
               c('V4','V5','V4',paste0('IRN_', toupper(selirns))),
               skip_absent = TRUE)
      
      if ('V5' %in% names(dt)) {
        dt[, V3 := as.character(V3)]
        dt[, V5 := as.numeric(V5)]
        
        # print(head(dt))
        
        return(dt)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }, vars = fn) %>%
    rbindlist(fill = TRUE)
}

# Read in selected variables
dt_mtf <- read_mtf('8th',
                   c('00030', # r sex
                     '00090', # father in household
                     '00310', # father ed lvl
                     '00320', # mother ed lvl
                     '07950', # equal job opp
                     '07930', # equal pay
                     '24770', # grade
                     '34700', # hrs gaming
                     '34730', # hrs social media
                     '34770', # hrs video weekday
                     '34780', # hrs video weekend
                     '00380', # religion importance
                     '00630', # dating
                     '05920', # hang out freq
                     '05925', # hang out freq
                     '25860'  # talk with parent
                     ),
                   dt_data.fn,
                   dt_cb.edit)

# 2019 survey mode
dt_mtf[, mode := 'NA']
dt_mtf[TABLET == 0, mode := 'paper']
dt_mtf[TABLET == 1, mode := 'tablet']

# social media hours
dt_mtf[, hrssocmed := 'NA']
dt_mtf[IRN_34730 %in% c(1,2), hrssocmed := '<1']
dt_mtf[IRN_34730 %in% c(3,4), hrssocmed := '1-4']
dt_mtf[IRN_34730 %in% c(5,6,7), hrssocmed := '5+']

# Watching videos: combine weekday and weekend
dt_mtf[, hrsvideowd := numeric(.N)]
dt_mtf[, hrsvideowd := NA]
dt_mtf[IRN_34770 %in% c(1), hrsvideowd := 0.0]
dt_mtf[IRN_34770 %in% c(2), hrsvideowd := 0.5]
dt_mtf[IRN_34770 %in% c(3), hrsvideowd := 1.5]
dt_mtf[IRN_34770 %in% c(4), hrsvideowd := 3.5]
dt_mtf[IRN_34770 %in% c(5), hrsvideowd := 5.5]
dt_mtf[IRN_34770 %in% c(6), hrsvideowd := 7.5]
dt_mtf[IRN_34770 %in% c(7), hrsvideowd := 10]

dt_mtf[, hrsvideowe := numeric(.N)]
dt_mtf[, hrsvideowe := NA]
dt_mtf[IRN_34780 %in% c(1), hrsvideowe := 0.0]
dt_mtf[IRN_34780 %in% c(2), hrsvideowe := 0.5]
dt_mtf[IRN_34780 %in% c(3), hrsvideowe := 1.5]
dt_mtf[IRN_34780 %in% c(4), hrsvideowe := 3.5]
dt_mtf[IRN_34780 %in% c(5), hrsvideowe := 5.5]
dt_mtf[IRN_34780 %in% c(6), hrsvideowe := 7.5]
dt_mtf[IRN_34780 %in% c(7), hrsvideowe := 10]

dt_mtf[, hrsvideo := 'NA']
dt_mtf[(5 * hrsvideowd + 2 * hrsvideowe) / 7 < 2, hrsvideo := '<2']
dt_mtf[(5 * hrsvideowd + 2 * hrsvideowe) / 7 >= 2, hrsvideo := '2-4']
dt_mtf[(5 * hrsvideowd + 2 * hrsvideowe) / 7 >= 4, hrsvideo := '4+']

# gaming hours
dt_mtf[, hrsgaming := 'NA']
dt_mtf[IRN_34700 %in% c(1,2), hrsgaming := '<1']
dt_mtf[IRN_34700 %in% c(3,4), hrsgaming := '1-4']
dt_mtf[IRN_34700 %in% c(5,6,7), hrsgaming := '5+']

# hang out with friends
dt_mtf[, ofthang := 'NA']
dt_mtf[IRN_05920 %in% c(1,2,3), ofthang := 'No']
dt_mtf[IRN_05920 %in% c(4,5), ofthang := 'Yes']
dt_mtf[IRN_05925 %in% c(1,2,3), ofthang := 'No']
dt_mtf[IRN_05925 %in% c(4,5,6), ofthang := 'Yes']

# dating
dt_mtf[, oftdate := 'NA']
dt_mtf[IRN_00630 %in% c(1), oftdate := 'No']
dt_mtf[IRN_00630 %in% c(2,3,4,5,6), oftdate := 'Yes']

# father in household
dt_mtf[, poppres := 'NA']
dt_mtf[IRN_00090 %in% c(0), poppres := 'Not present']
dt_mtf[IRN_00090 %in% c(1), poppres := 'Present']

# mom education
dt_mtf[, momeduc := 'NA']
dt_mtf[IRN_00320 %in% c(1,2,3,4), momeduc := 'Non-college']
dt_mtf[IRN_00320 %in% c(5,6), momeduc := 'College']

# talk to parents about problems
dt_mtf[, talkpar := 'NA']
dt_mtf[IRN_25860 %in% c(1,2), talkpar := 'No']
dt_mtf[IRN_25860 %in% c(3), talkpar := 'Yes']

# religion importance
dt_mtf[, religimp := 'NA']
dt_mtf[IRN_00380 %in% c(1,2), religimp := 'No']
dt_mtf[IRN_00380 %in% c(3,4), religimp := 'Yes']

svy_mtf <- as_survey(dt_mtf, weights = V5, strata = year)

# overall trend
dt_mtf.sum.totCA <- svy_mtf %>%
  filter(IRN_00030 == 1) %>%
  group_by(year) %>%
  summarise(
    variable = 'total',
    value = 'Agree',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.totMA <- svy_mtf %>%
  filter(IRN_00030 == 1) %>%
  group_by(year) %>%
  summarise(
    variable = 'total',
    value = 'Mostly agree or agree',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 4:5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 4:5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.tot <- rbind(dt_mtf.sum.totCA, dt_mtf.sum.totMA)

dt_mtf.sum.tot2 <- dt_mtf.sum.tot %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.tot2, here('d3','mtf-gender-total.csv'))

# Social network hours
dt_mtf.sum.smd <- svy_mtf %>%
  filter(IRN_00030 == 1 & (hrssocmed != 'NA' | year <= 2017)) %>%
  group_by(year, value = hrssocmed) %>%
  summarise(
    variable = 'hrssocmed',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.smd2 <- dt_mtf.sum.smd %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.smd2, here('d3','mtf-gender-socmed.csv'))

# Video hours
dt_mtf.sum.vid <- svy_mtf %>%
  filter(IRN_00030 == 1 & (hrsvideo != 'NA' | year <= 2017)) %>%
  group_by(year, value = hrsvideo) %>%
  summarise(
    variable = 'video',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.vid2 <- dt_mtf.sum.vid %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.vid2, here('d3','mtf-gender-video.csv'))

# Gaming hours
dt_mtf.sum.gam <- svy_mtf %>%
  filter(IRN_00030 == 1 & (hrsgaming != 'NA' | year <= 2017)) %>%
  group_by(year, value = hrsgaming) %>%
  summarise(
    variable = 'hrsgaming',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.gam2 <- dt_mtf.sum.gam %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.gam2, here('d3','mtf-gender-gaming.csv'))

# Hanging out with friends
dt_mtf.sum.hng <- svy_mtf %>%
  filter(IRN_00030 == 1 & (ofthang != 'NA')) %>%
  group_by(year, value = ofthang) %>%
  summarise(
    variable = 'ofthang',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.hng2 <- dt_mtf.sum.hng %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.hng2, here('d3','mtf-gender-hanging.csv'))

# Dating activity
dt_mtf.sum.dte <- svy_mtf %>%
  filter(IRN_00030 == 1 & (oftdate != 'NA')) %>%
  group_by(year, value = oftdate) %>%
  summarise(
    variable = 'oftdate',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.dte2 <- dt_mtf.sum.dte %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.dte2, here('d3','mtf-gender-dating.csv'))

# Father's presence
dt_mtf.sum.pop <- svy_mtf %>%
  filter(IRN_00030 == 1 & (oftdate != 'NA')) %>%
  group_by(year, value = oftdate) %>%
  summarise(
    variable = 'oftdate',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.pop2 <- dt_mtf.sum.pop %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.pop2, here('d3','mtf-gender-poppres.csv'))

# Mom education
dt_mtf.sum.med <- svy_mtf %>%
  filter(IRN_00030 == 1 & (oftdate != 'NA')) %>%
  group_by(year, value = oftdate) %>%
  summarise(
    variable = 'oftdate',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.med2 <- dt_mtf.sum.med %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.med2, here('d3','mtf-gender-momeduc.csv'))

# Talking to parents
dt_mtf.sum.par <- svy_mtf %>%
  filter(IRN_00030 == 1 & (oftdate != 'NA')) %>%
  group_by(year, value = oftdate) %>%
  summarise(
    variable = 'oftdate',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.par2 <- dt_mtf.sum.par %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.par2, here('d3','mtf-gender-parent.csv'))

# Religion
dt_mtf.sum.rlg <- svy_mtf %>%
  filter(IRN_00030 == 1 & (religimp != 'NA')) %>%
  group_by(year, value = religimp) %>%
  summarise(
    variable = 'religimp',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.rlg2 <- dt_mtf.sum.rlg %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.rlg2, here('d3','mtf-gender-relig.csv'))

# Survey mode
dt_mtf.sum.mode <- svy_mtf %>%
  filter(IRN_00030 == 1) %>%
  group_by(year, value = mode) %>%
  summarise(
    variable = 'mode',
    N = sum(!is.na(IRN_07950) & IRN_07950 %in% 1:5),
    jobop = survey_ratio(IRN_07950 %in% 5, IRN_07950 %in% 1:5, vartype = c('ci')),
    eqpay = survey_ratio(IRN_07930 %in% 5, IRN_07930 %in% 1:5, vartype = c('ci'))
  ) %>%
  data.table()

dt_mtf.sum.mode2 <- dt_mtf.sum.mode %>%
  melt(id.vars = c('year','variable','value','N'),
       variable.factor = FALSE, variable.name = 'colname', value.name = 'colval') %>%
  .[, question := substr(colname,1,5)] %>%
  .[, measure := substr(paste0(colname,'_pct'),7,9)] %>%
  dcast(year + variable + value + question + N ~ measure, value.var = 'colval')

fwrite(dt_mtf.sum.gam2, here('d3','mtf-gender-gaming.csv'))


