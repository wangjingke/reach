# functions used in the MATCH EMA processing
library(XLConnect)
source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_keys.R") # keys for creating surveys
# keys.child
# keys.mother

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_housekeeping.R") # housekeeping procedures
# ema.wave
# ema.fetchPrompt
# ema.compliance
# ema.waketime
# ema.triage
# ema.battery

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_promptList.R") # create EMA skeleton
# ema.indivPromptList
# ema.promptList

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_survey.R") # transform surveys to dataset
# ema.survey

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_manual.R") # manually aggregate and integrate surveys
# ema.manualAggregate
# ema.manualFetch

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_factors.R") # change multiple choices to factors
# ema.factor

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_accelerometer.R") # integrating ACC with EMA
# ema.attachACC
# ema.attachOACC
# ema.emaAccMatch
# ema.extractACC

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_anchor.R") # create anchored variables
# ema.anchor
# ema.anchor.parallel

source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_stata.R") # create stata labels
# ema.stata
