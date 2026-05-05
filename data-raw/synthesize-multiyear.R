#!/usr/bin/env Rscript
# Synthesize a 3-year multiyear sample dataset for Visible Explorer.
#
# Story arc baked into the data:
#   ~Year -3  Post-viral onset. Severe; mostly housebound.
#   Year -2   Slow improvement, many crashes. Pacing/rehab around month 12.
#   Mid Y -1  Setback after a re-infection — a clear, detectable change point.
#   Year 0    Climb back to a steadier baseline; pacing visibly better.
#
# The script writes to ../sample-visible-multiyear.csv relative to itself.

suppressPackageStartupMessages({
     library(dplyr)
     library(tidyr)
})

set.seed(20260504L)

end_date <- as.Date("2026-04-30")
start_date <- end_date - as.integer(365 * 3)
dates <- seq.Date(start_date, end_date, by = 1)
n <- length(dates)
day_idx <- seq_along(dates)
t <- (day_idx - 1) / (n - 1)

dow <- as.integer(format(dates, "%u"))

logistic <- function(x, k = 8, mid = 0.45) 1 / (1 + exp(-k * (x - mid)))

trend <- 1.6 + 2.6 * logistic(t, k = 6.5, mid = 0.40)

setback_center <- 0.62
setback_width <- 0.06
setback <- -1.6 * exp(-((t - setback_center) / setback_width)^2)

annual <- 0.18 * sin(2 * pi * t * 3 - pi / 3)

n_crashes <- 22
crash_centers <- sort(sample(seq.int(15, n - 15), n_crashes))
crash_widths <- sample(5:18, n_crashes, replace = TRUE)
crash_depths <- runif(n_crashes, 0.55, 1.4)
crash <- numeric(n)
for (i in seq_along(crash_centers)) {
     d <- (day_idx - crash_centers[i]) / crash_widths[i]
     crash <- crash - crash_depths[i] * exp(-(d^2))
}

weekend_lift <- ifelse(dow >= 6, 0.18, -0.04)

stab_lat <- trend + setback + annual + crash + weekend_lift + rnorm(n, 0, 0.32)
stab_lat <- pmin(5, pmax(0.4, stab_lat))

stab_score <- pmax(1L, pmin(5L, as.integer(round(stab_lat))))

sleep_lat <- (stab_lat - 1.5) / 3.5 + rnorm(n, 0, 0.45)
sleep_score <- as.integer(cut(
     sleep_lat,
     breaks = c(-Inf, 0.05, 0.75, Inf),
     labels = FALSE
)) -
     1L

hrv <- round(38 + 6 * stab_lat + rnorm(n, 0, 4))
hrv <- pmax(28L, pmin(78L, as.integer(hrv)))

rhr <- round(78 - 4.2 * stab_lat + rnorm(n, 0, 3.2))
rhr <- pmax(48L, pmin(82L, as.integer(rhr)))

cycle_len <- 28
cycle_phase <- (day_idx - 1) %% cycle_len
period_active <- cycle_phase < 5
period_value <- as.integer(period_active)

pms_factor <- ifelse(
     cycle_phase >= cycle_len - 4 & cycle_phase <= cycle_len - 1,
     0.9,
     0
)

symptom_load <- (3 - stab_lat * 0.6) + pms_factor + rnorm(n, 0, 0.35)
symptom_load <- pmax(0, symptom_load)

draw_symptom <- function(load, p_zero, severity_mult = 1) {
     z_thresh <- p_zero - 0.18 * load
     z_thresh <- pmax(0.02, pmin(0.98, z_thresh))
     u <- runif(length(load))
     raw <- ifelse(
          u < z_thresh,
          0,
          pmin(3, round(severity_mult * load * runif(length(load), 0.6, 1.4)))
     )
     as.integer(raw)
}

symptoms <- list(
     list(name = "Fatigue", cat = "General", p_zero = 0.10, m = 1.05),
     list(name = "Brain Fog", cat = "Brain", p_zero = 0.45, m = 0.95),
     list(name = "Headache", cat = "Brain", p_zero = 0.55, m = 0.95),
     list(name = "Dizziness", cat = "Brain", p_zero = 0.60, m = 0.85),
     list(name = "Lightheadedness", cat = "Brain", p_zero = 0.55, m = 0.90),
     list(name = "Memory issues", cat = "Brain", p_zero = 0.60, m = 0.85),
     list(name = "Anxiety", cat = "Brain", p_zero = 0.65, m = 0.85),
     list(name = "Depression", cat = "Brain", p_zero = 0.70, m = 0.80),
     list(name = "Muscle aches", cat = "Muscles", p_zero = 0.40, m = 1.05),
     list(name = "Muscle weakness", cat = "Muscles", p_zero = 0.55, m = 0.90),
     list(name = "Joint pain", cat = "Pain", p_zero = 0.55, m = 0.90),
     list(name = "Nerve pain", cat = "Pain", p_zero = 0.75, m = 0.75),
     list(name = "Stomach pain", cat = "Pain", p_zero = 0.70, m = 0.80),
     list(name = "Nausea", cat = "Gastrointestinal", p_zero = 0.70, m = 0.80),
     list(
          name = "Constipation",
          cat = "Gastrointestinal",
          p_zero = 0.75,
          m = 0.70
     ),
     list(name = "Diarrhea", cat = "Gastrointestinal", p_zero = 0.85, m = 0.65),
     list(
          name = "Acid Reflux",
          cat = "Gastrointestinal",
          p_zero = 0.80,
          m = 0.70
     ),
     list(
          name = "Lack of appetite",
          cat = "Gastrointestinal",
          p_zero = 0.70,
          m = 0.80
     ),
     list(
          name = "Palpitations",
          cat = "Heart and Lungs",
          p_zero = 0.65,
          m = 0.85
     ),
     list(
          name = "Shortness of breath",
          cat = "Heart and Lungs",
          p_zero = 0.65,
          m = 0.90
     ),
     list(name = "Light sensitivity", cat = "Sensory", p_zero = 0.50, m = 0.95),
     list(name = "Noise sensitivity", cat = "Sensory", p_zero = 0.50, m = 0.95),
     list(name = "Blurred vision", cat = "Sensory", p_zero = 0.75, m = 0.75),
     list(name = "Allergies", cat = "General", p_zero = 0.85, m = 0.65),
     list(name = "Crash", cat = "Experience", p_zero = 0.85, m = 1.10),
     list(
          name = "Emotionally stressful",
          cat = "Emotional",
          p_zero = 0.65,
          m = 0.85
     ),
     list(
          name = "Mentally demanding",
          cat = "Cognitive",
          p_zero = 0.50,
          m = 1.00
     ),
     list(name = "Socially demanding", cat = "Social", p_zero = 0.65, m = 0.95),
     list(name = "Physically active", cat = "Physical", p_zero = 0.55, m = 1.00)
)

physact_p_zero <- pmax(0.10, 0.85 - 0.18 * stab_lat)
mentdem_p_zero <- pmax(0.20, 0.75 - 0.10 * stab_lat)

build_obs <- function() {
     rows <- list()
     add <- function(date, tracker, category, value) {
          rows[[length(rows) + 1L]] <<- data.frame(
               observation_date = date,
               tracker_name = tracker,
               tracker_category = category,
               observation_value = value,
               stringsAsFactors = FALSE
          )
     }

     for (i in seq_len(n)) {
          add(dates[i], "Sleep", "Sleep", sleep_score[i])
          add(dates[i], "HRV", "Measurement", hrv[i])
          add(dates[i], "Resting HR", "Measurement", rhr[i])
          add(dates[i], "Stability Score", "Measurement", stab_score[i])
          if (period_value[i] == 1L) {
               add(dates[i], "Period", "Experience", 1L)
          }
     }

     for (s in symptoms) {
          if (s$name == "Physically active") {
               vals <- draw_symptom(stab_lat * 0.9, physact_p_zero, s$m)
          } else if (s$name == "Mentally demanding") {
               vals <- draw_symptom(stab_lat * 0.7 + 0.3, mentdem_p_zero, s$m)
          } else {
               vals <- draw_symptom(symptom_load, s$p_zero, s$m)
          }
          keep <- runif(n) < 0.92
          for (i in seq_len(n)) {
               if (!keep[i]) {
                    next
               }
               if (vals[i] == 0L && runif(1) < 0.35) {
                    next
               }
               add(dates[i], s$name, s$cat, vals[i])
          }
     }

     rows
}

rows <- build_obs()

funcap_items <- list(
     list(
          name = "Walking a short distance indoors from one room to another",
          cat = "Funcap_walking",
          base = 5.5
     ),
     list(
          name = "Walking between approx. 100m and 1km on level ground (length of 1 to 10 football fields)",
          cat = "Funcap_walking",
          base = 3.0
     ),
     list(
          name = "Physical activity with increased heart rate for approx. 15 min",
          cat = "Funcap_walking",
          base = 1.5
     ),
     list(
          name = "Standing up for approx. 5 minutes e.g. while queuing or while cooking",
          cat = "Funcap_upright",
          base = 4.0
     ),
     list(
          name = "Sitting in an upright chair (dining chair) with feet on floor for approx. 2 hours",
          cat = "Funcap_upright",
          base = 3.5
     ),
     list(
          name = "Sitting in bed for approx. ½ hour",
          cat = "Funcap_upright",
          base = 5.5
     ),
     list(name = "Showering standing up", cat = "Funcap_hygiene", base = 3.5),
     list(
          name = "Getting dressed in regular clothes",
          cat = "Funcap_hygiene",
          base = 5.0
     ),
     list(
          name = "Using the toilet (not bedpan or bedside commode)",
          cat = "Funcap_hygiene",
          base = 5.8
     ),
     list(
          name = "Reading a short text such as a mobile phone text message",
          cat = "Funcap_concentration",
          base = 5.2
     ),
     list(
          name = "Reading and understanding a non-fiction text such as an official document one page long",
          cat = "Funcap_concentration",
          base = 3.8
     ),
     list(
          name = "Focusing on a task for approx. 10 minutes continuously",
          cat = "Funcap_concentration",
          base = 4.0
     ),
     list(
          name = "Focusing on a task for approx. 2 hours continuously",
          cat = "Funcap_concentration",
          base = 1.8
     ),
     list(
          name = "Managing a full working day (non-physical work such as office work classes or lectures)",
          cat = "Funcap_concentration",
          base = 1.0
     ),
     list(
          name = "Using social media to stay in touch with others",
          cat = "Funcap_concentration",
          base = 4.6
     ),
     list(
          name = "Having a conversation for approx. 5 minutes",
          cat = "Funcap_communication",
          base = 5.0
     ),
     list(
          name = "Participating in a conversation with three people for approx. ½ hour",
          cat = "Funcap_communication",
          base = 3.0
     ),
     list(
          name = "Participating in a dinner party party or family event",
          cat = "Funcap_communication",
          base = 1.8
     ),
     list(
          name = "Stepping right outside your home",
          cat = "Funcap_outside",
          base = 4.5
     ),
     list(
          name = "Going to a shop for groceries",
          cat = "Funcap_outside",
          base = 2.5
     ),
     list(
          name = "Using public transport (bus or train)",
          cat = "Funcap_outside",
          base = 1.5
     ),
     list(
          name = "Participating in organized leisure activities such as classes sports etc.",
          cat = "Funcap_outside",
          base = 1.2
     ),
     list(
          name = "Cooking a complicated meal from scratch approx. 1 hour of preparation",
          cat = "Funcap_home",
          base = 2.5
     ),
     list(
          name = "Heavier housework (washing floors vacuuming etc.) for approx. ½ hour continuously",
          cat = "Funcap_home",
          base = 1.8
     ),
     list(
          name = "Staying in a room with normal lighting without sunglasses for approx. 1 hour",
          cat = "Funcap_light",
          base = 4.5
     ),
     list(
          name = "Staying outdoors in daylight without sunglasses for approx. 2 hours",
          cat = "Funcap_light",
          base = 3.0
     ),
     list(
          name = "Staying in a noisy environment (shopping mall café or open plan office) for approx. 1 hour",
          cat = "Funcap_light",
          base = 2.0
     )
)

funcap_dates <- seq.Date(
     from = as.Date(format(start_date, "%Y-%m-01")),
     to = end_date,
     by = "month"
)
funcap_dates <- funcap_dates[
     funcap_dates >= start_date &
          funcap_dates <= end_date
]
funcap_capacity_at <- function(d) {
     i <- match(as.character(d), as.character(dates))
     stab_lat[i] / 5
}

funcap_rows <- list()
for (k in seq_along(funcap_dates)) {
     fd <- funcap_dates[k]
     cap <- funcap_capacity_at(fd)
     for (item in funcap_items) {
          val <- item$base * (0.4 + 0.9 * cap) + rnorm(1, 0, 0.5)
          val <- as.integer(pmax(0L, pmin(6L, round(val))))
          funcap_rows[[length(funcap_rows) + 1L]] <- data.frame(
               observation_date = fd,
               tracker_name = item$name,
               tracker_category = item$cat,
               observation_value = val,
               stringsAsFactors = FALSE
          )
     }
}

notes_raw <- list(
     list(off = 0.005, text = "Got sick. Suspected COVID."),
     list(
          off = 0.020,
          text = "Still flat. Resting heart rate stays high after standing up."
     ),
     list(
          off = 0.045,
          text = "GP appt — bloods normal. Referred to long-COVID clinic."
     ),
     list(
          off = 0.090,
          text = "Bedbound week. Headache and post-exertional malaise after a 5-min walk."
     ),
     list(
          off = 0.140,
          text = "First short walk to the kitchen window without crashing the next day."
     ),
     list(off = 0.190, text = "Started a pacing journal."),
     list(off = 0.260, text = "Cardiologist confirmed POTS. Started LDN."),
     list(
          off = 0.310,
          text = "First time outside in months — sat on the balcony for 10 min."
     ),
     list(
          off = 0.355,
          text = "Crashed after a phone call with family. Note: limit to 15 min."
     ),
     list(off = 0.410, text = "Pacing class week 1 — heart rate cap of 105."),
     list(
          off = 0.465,
          text = "Walked 200m on flat ground without crashing the next day."
     ),
     list(off = 0.520, text = "Best week so far. Showered standing up twice."),
     list(
          off = 0.585,
          text = "Re-infection. Tested positive again. Bracing for setback."
     ),
     list(
          off = 0.620,
          text = "Setback confirmed — back to bedbound most days."
     ),
     list(
          off = 0.660,
          text = "Slowly back to baseline pacing. Heart rate cap dropped to 95 for now."
     ),
     list(
          off = 0.715,
          text = "Walked 100m again. A different body and a different starting line."
     ),
     list(
          off = 0.770,
          text = "Read a short novel chapter without brain fog crash."
     ),
     list(off = 0.820, text = "Cooked dinner sitting down. Felt like a win."),
     list(
          off = 0.870,
          text = "First social visit at home — one friend for one hour. Slept 11h after."
     ),
     list(
          off = 0.910,
          text = "Tried a 15-minute gentle yoga video lying down. Tolerated."
     ),
     list(
          off = 0.945,
          text = "Started tracking mornings vs afternoons separately in my head."
     ),
     list(off = 0.975, text = "Two-week steady stretch. First time in years.")
)

notes_rows <- list()
for (nv in notes_raw) {
     i <- max(1L, min(n, as.integer(round(nv$off * n))))
     txt <- gsub(",", "", nv$text, fixed = TRUE)
     notes_rows[[length(notes_rows) + 1L]] <- data.frame(
          observation_date = dates[i],
          tracker_name = "Note",
          tracker_category = "Note",
          observation_value = txt,
          stringsAsFactors = FALSE
     )
}

all_rows <- c(rows, funcap_rows, notes_rows)
csv <- do.call(rbind, all_rows)

csv$observation_value <- as.character(csv$observation_value)

date_order <- order(
     csv$observation_date,
     match(
          csv$tracker_name,
          c("Sleep", "HRV", "Resting HR", "Stability Score")
     ),
     csv$tracker_name
)
csv <- csv[date_order, ]

csv$observation_date <- format(csv$observation_date)

out_path <- if (file.exists("../_quarto.yml")) {
     "../sample-visible-multiyear.csv"
} else {
     "sample-visible-multiyear.csv"
}

write.csv(csv, out_path, row.names = FALSE, quote = FALSE)

message(sprintf(
     "Wrote %s rows spanning %s to %s",
     format(nrow(csv), big.mark = ","),
     min(csv$observation_date),
     max(csv$observation_date)
))
