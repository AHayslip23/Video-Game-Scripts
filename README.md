# Video Game Scripts

An R exploratory analysis of video game sales (16,718 games, with regional sales, critic and user scores, and platform), focused on Nintendo's sales history.

## Repository Contents

| File | Description |
|---|---|
| `video games script.R` | R script with the full analysis and charts |
| `video game sales.csv` | Video game sales dataset |

## Commit History

### `dae569c`: Upload of the R script and dataset (2022-05-18)
- **Regional sales correlations for Nintendo games:**
  - Europe vs. North America: 81%, a strong relationship
  - Japan vs. North America: 58%, weak
  - Japan vs. Europe: 59%, weak
  - North American vs. global sales: about 95–98%
- **Market leaders:** compared Nintendo, Microsoft and Sony in North America, where Microsoft leads.
- **Nintendo platforms:**
  - most games sell under $2 million globally
  - the Wii and DS were Nintendo's best-selling platforms, and the Wii U was the worst
  - handhelds generally outsell home consoles
  - the Game Boy had a 10-year run, sometimes outselling the NES
  - the DS made up about 75% of Nintendo's 2005 revenue
- **Genre and release trends over time:**
  - platformers dominated the late '80s and early '90s
  - sports and platformers carried 2006 and 2009 (Wii Sports and Mario)
  - music and misc games peaked from 2006 to 2010
  - Nintendo's yearly game releases peaked at 600–700 between 2006 and 2010
- **Score vs. sales:** critic score vs. global sales for Nintendo games showed a 15.9% correlation, which isn't a linear relationship.

## Tools
R, RStudio, dplyr, ggplot2
