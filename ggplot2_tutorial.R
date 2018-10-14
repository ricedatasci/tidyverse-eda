library(ggplot2)



#tidyr
#Each variable must have its own column.
#Each observation must have its own row.
#Each value must have its own cell.

n=10
wide <- data.frame(
  ID = c(1:n),
  Face.1 = c(411,723,325,456,579,612,709,513,527,379),
  Face.2 = c(123,300,400,500,600,654,789,906,413,567),
  Face.3 = c(1457,1000,569,896,956,2345,780,599,1023,678)
)

long <- wide %>% gather(Face, ResponseTime, Face.1:Face.3)

long_separate <- long %>% separate(Face, c("Target", "Number"))

long_unite <- long_separate %>% unite(Face, Target, Number, sep = ".")

back_to_wide <- long_unite %>% spread(Face, ResponseTime)

#dplyr
msleep <- read.csv('msleep.csv')
sleepData <- select(msleep, name, sleep_total)
head(select(msleep, -name))
head(select(msleep, name:order))
head(select(msleep, starts_with("sl")))
#ends_with() = Select columns that end with a character string
#contains() = Select columns that contain a character string
#matches() = Select columns that match a regular expression
#one_of() = Select columns names that are from a group of names
filter(msleep, sleep_total >= 16)
filter(msleep, order %in% c("Perissodactyla", "Primates"))
msleep %>% arrange(order) %>% head
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>% 
  filter(sleep_total >= 16)

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

msleep %>% 
  summarise(avg_sleep = mean(sleep_total))

msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())


#ggplot2

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()


#LAYERS

# Data Layer
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  
  # Geometries Layer 
  geom_jitter(alpha = 0.6)

p <- p +  
  ## Facets (optional)
  facet_grid(. ~ Species) +
  
  ## Statistics (optional)
  stat_smooth(method = "lm", se = F, col = "red") + 
  
  ## Coordinates Layer (optional)
  scale_y_continuous("Sepal Width (cm)", limits = c(2,5), expand = c(0,0)) + 
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), expand = c(0,0)) + v
  coord_equal()

p <- p + 
  ## Theme Layer (optional)
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.ticks = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        strip.text = element_blank(), 
        panel.margin = unit(1, "lines")
  )



#AESTHETICS

ggplot(diamonds, aes(x = carat, y = price) +
  geom_point()
  
ggplot(diamonds, aes(x = carat, y = price, col=color) )+
         geom_point()

ggplot(diamonds, aes(x = carat, y = price, shape=cut) )+
  geom_point()

ggplot(diamonds, aes(x = carat, y = price))+
  geom_point(alpha = 0.4)

ggplot(diamonds, aes(x = carat, y = price, col=clarity))+
  geom_point(alpha = 0.4)

#GEOMETRY
ggplot(diamonds, aes(x = carat, y = price)) +
         geom_smooth()
ggplot(diamonds, aes(x = carat, y = price, col=clarity)) +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_line()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth() +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha =0.2) +
  geom_smooth(aes(col = clarity),se =FALSE)

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#AESTHETICS + GEOMETRIES
ggplot(mtcars, aes(x = wt, y = mpg, fill = factor(cyl), col = factor(am))) +
  geom_point(shape = 21, size = 4, alpha= .6, stroke = 1.5)
 
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_text(label = rownames(mtcars), color = 'red')

ggplot(mtcars, 
       aes(mpg, qsec, 
           col = factor(cyl), 
           shape = factor(am),
           size = (hp/wt)
       )) + 
  geom_point()

ggplot(diamonds, aes(clarity, carat, col = price)) + 
  geom_point(alpha = 0.5)
ggplot(diamonds, aes(clarity, carat, col = price)) + 
  geom_point(alpha = 0.5, position = "jitter")

#STATISTICS
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth()

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F)

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) + 
  stat_smooth(aes(group = 1), method = "lm", se = F)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

#POSITION
posn.d <- position_dodge(width = 0.1)
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)
posn.j <- position_jitter(width = 0.2)

wt.cyl.am<-
ggplot(mtcars, aes(cyl, wt, col = am, fill = am, group = am))+
geom_point(
  position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2), 
  alpha = 0.6
) 
wt.cyl.am + 
  stat_summary(
    fun.data = mean_sdl, 
    fun.args = list(mult = 1), 
    position = position_dodge(width = 0.1)
  )

#COORDINATES

p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + geom_point() + geom_smooth()

p + coord_cartesian(xlim = c(3,6))

base.plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F)

base.plot + coord_equal()

#Pie Charts!!

thin.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar()

thin.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar()

#FACETS

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

p + facet_grid(am ~ .)

p + facet_grid(. ~ cyl)

p + facet_grid(am ~ cyl)


#THEMES

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species))+
  geom_point()
p + theme_classic()
p + theme_economist() +
  scale_color_economist()
p+ theme_wsj()+ scale_colour_wsj("colors6")
p + theme_tufte()

z <- ggplot(mtcars, aes(wt, mpg, col = cyl)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = F) + 
  facet_grid(. ~ cyl)

myPink <- "#FEE0D2"
z + theme(
  plot.background = element_rect(fill = myPink))

z + theme(
  plot.background = element_rect(
    fill = myPink, 
    color = "black",
    size = 3))

uniform_panels <- theme(panel.background = element_blank(), 
                        legend.key = element_blank(), 
                        legend.background=element_blank(), 
                        strip.background = element_blank())

z<-z + 
  theme(
    plot.background = element_rect(
      fill = myPink, 
      color = "black",
      size = 3)) +
  uniform_panels

z <- z + theme(
  panel.grid = element_blank(),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black")
)
myRed <- "#99000D"

z <- z + theme(
  strip.text = element_text(size = 16, color = myRed),
  axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
  axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
  axis.text = element_text(color = "black")
)

z + theme(
  legend.direction = "horizontal"
)

z + theme(
  legend.position = "bottom"
)

library(grid)

z + theme(
  panel.spacing.x = unit(2, "cm"),
  plot.margin = unit(c(0,0,0,0), "cm")
)
