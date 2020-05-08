Det är inte nödvändigt att läsa följande stycken för att använda programmet, så de kan med gott samvete hoppas över om inte användaren behöver förstå den underliggande beräkningsmetodiken.

Programmet beräknar diskonteringsräntekurvorna genom att lösa följande optimeringsproblem.

$$\min_{x}\left(\text{par}(t_3)\cdot\sum_{i=1}^{t_1}\text{DF}(i)+y_{t_1+1}(x)+\dots+y_{t_3-1}(x)+\frac{1}{(1+x)^{t_3}}-\left(1-\frac{1}{1+x}\right)\right)^2$$

För fler detaljer, se beräkningsmetodik i promemoria [FI-Dnr 13-11409 (2013-12-01)].