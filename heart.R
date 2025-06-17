library('RHRV')
library('tidyverse')

data_raw_file <- "/Users/kiero/Dropbox/devel/Garmin_HRV/heart_rate_log.csv"
data <- read_delim(file = data_raw_file, delim = ',') |>
  select(-Energy_Expended)
data |> 
  mutate(RR_Intervals = gsub("\\[|\\]", "", RR_Intervals)) |>
  separate(col = RR_Intervals, into = paste0('RR', 1:5), sep = ',') |>
  mutate_at(paste0('RR', 1:5), as.numeric) |>
  pivot_longer(cols = paste0('RR', 1:5), names_to = 'RRid', values_to = 'RR') |>
  mutate(computed_hr = 60/RR) |>
  mutate(hr_ratio = Heart_Rate/computed_hr) -> 
  data_long
  
data_long |> ggplot(mapping = aes(x = Heart_Rate, y = computed_hr)) +
  geom_point() +
  theme_minimal()

data_long |> ggplot(mapping = aes(x = Timestamp, y = hr_ratio)) +
  geom_point() +
  theme_minimal()

beats <- cumsum(c(0, na.omit(data_long$RR)))

hrv.data  <- CreateHRVData()
hrv.data <- SetVerbose(hrv.data, Verbose = T)
hrv.data <- LoadBeatVector(hrv.data, beats, scale = 1)
hrv.data <- BuildNIHR(hrv.data)
hrv.data <- FilterNIHR(hrv.data)
hrv.data <- InterpolateNIHR(hrv.data, freqhr = 4)
PlotNIHR(hrv.data, main = "niHR")
hrv.data <- CreateTimeAnalysis(hrv.data, size = 300, interval = 7.8125)
hrv.data <- CreateFreqAnalysis(hrv.data)
hrv.data <- CalculatePowerBand(hrv.data, indexFreqAnalysis= 1,
                               size = 300, shift = 30)
hrv.data <- CreateFreqAnalysis(hrv.data)
hrv.data <- CalculatePowerBand( hrv.data , indexFreqAnalysis = 2,
                      type = "wavelet", wavelet = "la8",
                      bandtolerance = 0.01, relative = F,
                      ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                      LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 2500, ymaxratio = 2)
PlotSpectrogram(HRVData=hrv.data, size=300, shift=60, sizesp=2048, freqRange = c(0,0.2))

# LF/HF > 1: Indicates a dominance of sympathetic activity over parasympathetic activity
freq_an1 <- hrv.data$FreqAnalysis[[1]] 
freq_tbl <- data.frame(Time = freq_an1$Time, HF = freq_an1$HF, LF = freq_an1$LF)
freq_tbl <- freq_tbl |> mutate(freq_ratio = LF/HF) |>
  mutate(dominant = if_else(freq_ratio > 1, 'sympathetic', 'parasympathetic'))

bg_color <- ifelse(freq_tbl$freq_ratio < 1, "slateblue", "tomato")
freq_tbl |> ggplot(mapping = aes(x = Time, y = freq_ratio)) +
  geom_line(colour = 'grey') +
  geom_hline(yintercept = 1, colour = 'lightgrey') +
  geom_point(mapping = aes(col = dominant)) +
  theme_minimal()
