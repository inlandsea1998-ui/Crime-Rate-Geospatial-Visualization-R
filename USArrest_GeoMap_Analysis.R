setwd('C:\\Users\\inlan')
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")
install.packages("gridExtra")
library(ggplot2)
library(maps)
library(dplyr)
library(gridExtra)
data("USArrests")

#Graph 1
states_map <- map_data("state") # 주별 경계 데이터 불러오기
USArrests$state <- tolower(state.name) # 주 이름을 소문자로 변환하여 USArrests
map_data <- merge(states_map, USArrests, by.x = "region", by.y = "state") %>%
  arrange(group, order)
map_data # 병합된 데이터 확인)
ggplot(map_data, aes(x=long,y=lat,group=group,fill=Murder))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="lightyellow",high="darkred",breaks=seq(min(map_data$Murder),max(map_data$Murder),length.out=6))+
  coord_fixed(1.3)+
  labs(title="미국 주별 살인율",fill="Murder Rate")+
  theme(axis.text=element_blank(),axis.ticks=element_blank())

#Graph 2
states_map <- map_data("state") # 주별 경계 데이터 불러오기
USArrests$state <- tolower(state.name) # 주 이름을 소문자로 변환하여 USArrests
median_assault <- median(USArrests$Assault)# ✅ merge 전에 color 열 추가!
USArrests$color <- ifelse(USArrests$Assault > median_assault, "red", "blue")
map_data <- merge(states_map, USArrests, by.x = "region", by.y = "state") %>%
  arrange(group, order)
map_data # 병합된 데이터 확인)
median_assault<-median(USArrests$Assault)
USArrests$color<-ifelse(USArrests$Assault>median_assault,"red","blue")
ggplot(map_data,aes(x=long,y=lat,group=group,fill=color))+
  geom_polygon(color="black")+
  scale_fill_manual(values=c("red"="red","blue"="blue"))+
  coord_fixed(1.3)+
  theme(axis.text=element_blank(),axis.ticks=element_blank())


#Graph 3
USArrests$state <- state.name  # 주 이름 추가

# ── 함수: 각 변수별 상위/하위 5개 추출 후 막대그래프 생성 ──────────────
plot_top_bottom <- function(var, label) {
  
  df <- USArrests %>% select(state, value = all_of(var))
  
  top5    <- df %>% arrange(desc(value)) %>% slice(1:5) %>% mutate(rank = "Top 5 (상위)")
  bottom5 <- df %>% arrange(value)       %>% slice(1:5) %>% mutate(rank = "Bottom 5 (하위)")
  combined <- bind_rows(top5, bottom5)
  combined$state <- factor(combined$state, levels = combined$state)
  
  ggplot(combined, aes(x = state, y = value, fill = rank)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Top 5 (상위)" = "#d73027", "Bottom 5 (하위)" = "#4575b4")) +
    labs(title = paste0(label, " - 상위/하위 5개 주"),
         x = "주(State)", y = label, fill = "구분") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

# ── 각 변수별 그래프 생성 ───────────────────────────────────────────────
p1 <- plot_top_bottom("Murder",   "살인율 (Murder)")
p2 <- plot_top_bottom("Assault",  "폭행율 (Assault)")
p3 <- plot_top_bottom("UrbanPop", "도시인구 (UrbanPop)")
p4 <- plot_top_bottom("Rape",     "강간율 (Rape)")

# ── 2x2 배치로 출력 ────────────────────────────────────────────────────
grid.arrange(p1, p2, p3, p4, ncol = 2)

# ── 수치 확인용 출력 ───────────────────────────────────────────────────
vars <- c("Murder", "Assault", "UrbanPop", "Rape")
labels <- c("살인율", "폭행율", "도시인구비율", "강간율")

for (i in seq_along(vars)) {
  df <- USArrests %>% select(state, value = all_of(vars[i]))
  cat("\n========================================\n")
  cat(labels[i], "상위 5개 주:\n")
  print(df %>% arrange(desc(value)) %>% slice(1:5))
  cat(labels[i], "하위 5개 주:\n")
  print(df %>% arrange(value) %>% slice(1:5))
}