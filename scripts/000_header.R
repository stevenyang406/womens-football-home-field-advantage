RAW  <- function(...) file.path("..", "data/raw", ...)
PROCESSED  <- function(...) file.path("..", "data/processed", ...)
TEMP  <- function(...) file.path("..", "temp", ...)
OUTPUT  <- function(...) file.path("..", "output", ...)

library(tidyverse)
library(StatsBombR)
library(arrow)
library(fixest)
