#!/usr/bin/env Rscript

library(ggplot2)

variants = c("cow_off", "cow_on", "mor_off", "mor_on")

data <- data.frame(
    rep = integer(),
    variant = integer(),
    type = character(),
    num_reqs = integer(),
    times = integer()
)

for (variant in variants) {
    for (type in c("scan", "update")) {
        num_reqs <- scan(paste0("trace_", type, "_", variant), what = integer(), quiet = TRUE)
        times <- scan(paste0("runtime_", type, "_", variant), what = integer(), quiet = TRUE)
        data <- rbind(data, data.frame(
            rep = 1:length(num_reqs),
            variant = variant,
            type = type,
            num_reqs = num_reqs,
            times = times
        ))
    }
}

data[["variant"]] <- factor(data[["variant"]], levels = variants, labels = c(
    "CoW with HMT and timeline server disabled",
    "CoW with HMT and timeline server enabled",
    "MoR with HMT and timeline server enabled",
    "MoR with HMT and timeline server disabled"
))

data[["type"]] <- factor(data[["type"]], levels = c("scan", "update"), labels = c("Table scan", "Update"))

p1 <- ggplot(data, aes(x = rep, y = num_reqs)) +
    geom_line(aes(color = type, linetype = type)) +
    ylim(0, NA) +
    labs(x = "Repetitions", y = "Number of S3 requests", color = "Type of operation", linetype = "Type of operation") +
    theme(legend.position = "top") +
    facet_wrap(vars(variant), ncol = 2)

ggsave("num_reqs.pdf", plot = p1, height = 10)
ggsave("num_reqs.svg", plot = p1, height = 10)

p2 <- ggplot(data, aes(x = rep, y = times)) +
    geom_line(aes(color = type, linetype = type)) +
    ylim(0, NA) +
    labs(x = "Repetitions", y = "Runtime [ms]", color = "Type of operation", linetype = "Type of operation") +
    theme(legend.position = "top") +
    facet_wrap(vars(variant), ncol = 2)

ggsave("times.pdf", plot = p2, height = 10)
ggsave("times.svg", plot = p2, height = 10)
