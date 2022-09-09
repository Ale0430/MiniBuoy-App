
Statistics = tibble(Parameter = c('SurveyDays', 'Frequency', 'MedianCurrent'), Target = c(15.4, 0.2, 0.12))
DATA = 'Target'

message(
  'The deployment length was ',
  Statistics %>%
    filter(Parameter == 'SurveyDays') %>%
    select(DATA), 
  ' days, ',
  if (Statistics %>%
      filter(Parameter == 'SurveyDays') %>%
      select(DATA) < 15) 
    { 'which is too short to provide robust results. Next time, we recommend a longer deployment of at least 15 days to represent a full spring-neap cycle.\n\n' 
    } else { 'which is long enough to provide robust results.\n\n' },
  'Inundation at the site appears to be ', 
  if (Statistics %>%
      filter(Parameter == 'Frequency') %>%
      select(DATA) %>%
      summarise(round(., digits = 0)) == 0)
    { 'continuous.\n\n' } else 
  if (Statistics %>%
      filter(Parameter == 'Frequency') %>%
      select(DATA) %>%
      summarise(round(., digits = 0)) == 1)
    { 'tidal (diurnal).\n\n' } else
  if (Statistics %>%
      filter(Parameter == 'Frequency') %>%
      select(DATA) %>%
      summarise(round(., digits = 0)) == 2)
    { 'tidal (semi-diurnal).\n\n' } else { 'mixed (i.e. no clear tidal signal).\n\n'}, 
  'Median current velocities are ',
  Statistics %>%
    filter(Parameter == 'MedianCurrent') %>%
    select(DATA) %>%
    summarise(round(., digits = 2)),
  ' m/s, so can be considered',
  if (Statistics %>%
      filter(Parameter == 'MedianCurrent') %>%
      select(DATA) > 0.1)
    { ' high enough to cause scour and dislodge seedlings.' } else { ' low enough to allow coastal plants to establish.'}
  )
