# Espresso machine output variation

The UBC Biodiversity Research Centre has a wonderful coffee co-op whereby students, faculty and staff can purchase coffee in-house for a minimal price. The heart of this operation is the espresso machine. Well, the heart is actually [Andrew MacDonald](https://github.com/aammd) who [singlehandedly runs the coffee co-op](https://github.com/aammd/CoffeeCoop), but anyway. Patrons can select "large", "medium", or "small" pulls depending on whether they desire a fuller cup or a single shot.

A long-standing question among coffee co-op patrons has been why the espresso machine seems to vary in the amount of coffee it dispenses for the same selection. I, being a regular "large" guy, recently began collecting data on variation in coffee volume dispensed for selection of a single "large" coffee. This repository contains the data (coffee-data.csv), R code used to visualize the data (coffee.R), and output figures.

# coffee-data.csv variables

**duration** = the amount of time (in seconds) the machine takes to dispense a single "large" coffee

**height** = Using the same mug every time, I measure the height (in mm) of the coffee in my mug

**volume** = volume of coffee (in ml), calculated from height assuming my mug is a perfect cylinder (which it effectively is--hooray geometry!)

**rate** = volume divided by duration

**day** = day of the week (mon-sun)	

**date** = date, in DD MMM YYYY format

**time** = time at which coffee was dispensed, to the nearest 5 min, in HH:MM

**workday** = binary, whether coffee was dispensed during a work day (mon thru fri) or a weekend (sat-sun)	

**warmup** = binary, whether the espresso machine had to "warm up" before dispensing coffee (i.e. if it hadn't been used in several hours).

**X** = notes, comments	

# Anticipated FAQ

**What do the red points in the figures represent?**

The red points indicate times where the machine had to warm up before dispensing coffee.
