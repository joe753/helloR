### try 1
d1 = mpg %>% 
  filter(year == 1999) %>% 
  group_by(year,displ) %>% 
  summarise(m1 = mean(cty), m2 = mean(hwy))
d2 = mpg %>% 
  filter(year == 2008) %>%
  group_by(year, displ) %>% 
  summarise(m1 = mean(cty), m2 = mean(hwy))
b = bind_cols(d1, d2)
View(b)

ggplot( b, aes(x=displ)) + 
  geom_line(aes(y=m1, color='1999 cty')) + 
  geom_line(aes(y=m2, color='1999 hwy')) +
  geom_line(aes(y=m11, color='2008 cty'), size=2) +
  geom_line(aes(y=m21, color='2008 hwy'), size=2) +
  scale_colour_manual("", breaks = c("1999 cty", "1999 hwy","2008 cty", "2008 hwy"),
                      values = c("red", "grey", "blue", "black")) +
  xlab("배기량") +
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 45)) +
  labs(title = '연도별 통합연비', subtitle = '굵은선 = 2008년') 

## try2

a = data  %>% group_by (cls, gen)%>% filter(kor >= 80)
a



ggplot(a, aes(cls)) +
  geom_bar(aes(fill=gen),
           width = 0.5) +
  theme(axis.text.x = element_text(angle=0,       # 글씨의 기울기
                                   vjust=0.6)) +   # 글씨의 하단 맞춤(띄우기)
  scale_fill_discrete(name = "성별") + # legend
  xlab("학급") + 
  ylab("학생수") + 
  labs(title = '국어 우수 학생', subtitle = '80점 이상')


### try3
try3 = data %>% group_by(cls) %>% filter(kor >= 95)
try3
ggplot(try3, aes(kor)) +
  geom_density(aes(fill=factor(cls)), alpha=0.5) +
  labs(title="반별 국어 우수 학생", subtitle = "(국어성적 A+)",
       caption="Source: ggplot2::mpg",
       x = "성적",
       y = "밀도",
       fill = "실린더수")

### try 4

try4 = midwest %>% filter(poptotal <= 500000 & popasian <= 10000)
try4

ggplot(data, aes(cls, kor)) +
  geom_point(aes(color=cls, size=kor), 
             alpha=0.3)


ggplot(try4, aes(poptotal, popasian)) +
  geom_point(aes(color=poptotal, size=popasian), 
             alpha=0.3)