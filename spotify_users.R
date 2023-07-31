
set.seed(42)

library(tidyverse)
options(repr.plot.width = 24.0, repr.plot.height = 18.3)
readxl::read_xlsx("/Users/fridc/Documents/spotify/Spotify_data.xlsx") -> data

data %>% mutate(pod_variety_satisfaction = case_when(pod_variety_satisfaction == "Very Satisfied" ~ 5,
                                                   pod_variety_satisfaction == "Satisfied" ~ 4, 
                                                   pod_variety_satisfaction == "Ok" ~ 3, 
                                                   pod_variety_satisfaction == "Dissatisfied" ~ 2, 
                                                   pod_variety_satisfaction == "Very Dissatisfied"~ 1)) -> data


stringr::str_to_title(data$pod_host_preference) -> data$pod_host_preference

stringr::str_to_title(data$fav_pod_genre) -> data$fav_pod_genre

stringr::str_to_title(data$pod_lis_frequency) -> data$pod_lis_frequency
# Split the string values and lengthen the frame
data%>%mutate(music_expl_method = strsplit(music_expl_method, split = ", "))%>%unnest(music_expl_method) -> data # Empty String Version
data%>%mutate(music_expl_method = strsplit(music_expl_method, split = ","))%>%unnest(music_expl_method) -> data
data%>%mutate(music_lis_frequency = strsplit(music_lis_frequency, split = ", "))%>%unnest(music_lis_frequency) -> data
data%>%mutate(music_lis_frequency = strsplit(music_lis_frequency, split = ","))%>%unnest(music_lis_frequency) -> data

data$music_lis_frequency<- stringr::str_to_title(data$music_lis_frequency)
data%>%mutate(music_Influencial_mood = strsplit(music_Influencial_mood, split = ", "))%>%unnest(music_Influencial_mood) -> data
data$music_Influencial_mood<- stringr::str_to_title(data$music_Influencial_mood)
data$fav_music_genre <- stringr::str_to_title(data$fav_music_genre)

data$spotify_subscription_plan <- ifelse(data$spotify_subscription_plan  == "Free (ad-supported)","Free","Premium") 
data%>%mutate(spotify_listening_device = strsplit(spotify_listening_device, split = ", "))%>%unnest(spotify_listening_device) -> data
data$spotify_listening_device <- stringr::str_to_title(data$spotify_listening_device)

data <- data%>%mutate_if(is.character, as.factor)
colnames(data) <- c("Age","Gender","Usage_Period","Listening_Device","Subscription",
                    "Subscription_Willingness","Plan_Type","Content","Favorite_Genre",
                    "Time_Slot","Mood","Frequency","Exploration_Method","Rating","Podcast_Frequency",
                    "Favorite_Podcast_Genre","Pref_Pod_Format","Pred_Host_Type","Pref_Pod_Duration","Variety_Satisfaction")

str(data)




data%>%select(c("Subscription","Subscription_Willingness","Plan_Type")) -> data_will

data_will%>%mutate(Target = case_when(Subscription == "Free" & 
                                        Subscription_Willingness == "Yes" ~ "T", 
                                      Subscription == "Free" & Subscription_Willingness == "No" ~ "C",
                                      Subscription == "Premium" & Subscription_Willingness == "No" ~ "T",
                                      Subscription == "Premium"  & Subscription_Willingness == "Yes" ~ "S"))%>%mutate_if(is.character, as.factor) -> data_will


data_will$Target -> data$Target


data%>%mutate_if(is.numeric, as.factor)%>%
  pivot_longer(cols = colnames(data[,!colnames(data) %in% "Target"]),
               values_to = "Value",
               names_to  = "Spec")%>%group_by(Spec)%>%
  ggplot(mapping =  aes(x = Value, fill = Target)) + geom_bar(position = "fill") + facet_wrap(~Spec, scale = "free") +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 3)) +  
  scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1", n = 4)) + 
  xlab("Properties") + 
  ylab("Percentage of Different Audience Groups") + 
  ggtitle("The Audience Distribution",subtitle = "Finding a Target Audience for Marketing")




data%>%mutate_if(is.numeric, as.factor)%>%pivot_longer(cols = colnames(data),
                                                       values_to = "Value", names_to = "Spec")%>%
  group_by(Spec)%>%arrange(Value)%>%
  ggplot(mapping = aes(x = Value, fill = Spec)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~Spec, scales = "free") + 
  theme_minimal() + scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) 


data%>%mutate_if(is.numeric,as.factor)%>%
  pivot_longer(cols = colnames(data)[!colnames(data) %in% "Age"], values_to = "Value",names_to = "Spec")%>%
  group_by(Age)%>% count(Value,Spec)%>%
  ggplot(mapping = aes(x = Value, y = n, fill = Age)) + geom_col() +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(3, "cm"),
        legend.key.width = unit(3, "cm"), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(vjust = 2)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Cavalcanti1", n = 5)) + facet_wrap(~Spec, scale = "free") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))


data%>%mutate_if(is.numeric,as.factor)%>%
  pivot_longer(cols = colnames(data)[!colnames(data) %in% "Gender"], values_to = "Value",names_to = "Spec")%>%
  group_by(Gender)%>% count(Value,Spec)%>%
  ggplot(mapping = aes(x = Value, y = n, fill = Gender)) + geom_col() +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Cavalcanti1", n = 5)) + facet_wrap(~Spec, scale = "free") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))

data%>%mutate_if(is.numeric,as.factor)%>%
  pivot_longer(cols = colnames(data)[!colnames(data) %in% "Usage_Period"], values_to = "Value",names_to = "Spec")%>%
  group_by(Usage_Period)%>% count(Value,Spec)%>%
  ggplot(mapping = aes(x = Value, y = n, fill = Usage_Period)) + geom_col() +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Moonrise1", n = 4)) + facet_wrap(~Spec, scale = "free") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))

data%>%mutate_if(is.numeric,as.factor)%>%
  pivot_longer(cols = colnames(data)[!colnames(data) %in% "Listening_Device"], values_to = "Value",names_to = "Spec")%>%
  group_by(Listening_Device)%>% count(Value,Spec)%>%
  ggplot(mapping = aes(x = Value, y = n, fill = Listening_Device)) + geom_col() +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + 
  scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1", n = 4)) + facet_wrap(~Spec, scale = "free") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))

data%>%mutate_if(is.numeric,as.factor)%>%
  pivot_longer(cols = colnames(data)[!colnames(data) %in% "Subscription"], values_to = "Value",names_to = "Spec")%>%
  group_by(Subscription)%>% count(Value,Spec)%>%
  ggplot(mapping = aes(x = Value, y = n, fill = Subscription)) + geom_col() +  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling2", n = 4)) + facet_wrap(~Spec, scale = "free") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE, n.dodge = 2))

data%>%ggplot(mapping  = aes(x = Usage_Period, fill = Subscription)) + 
  geom_bar(position = "fill") + theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling2", n = 4)) + xlab("Usage Period") + ylab("Percentage")

data%>%ggplot(mapping  = aes(x = Listening_Device, fill = Subscription)) + 
geom_bar(position = "fill") + theme_minimal() + 
theme(axis.text = element_text(size = 14), 
      axis.title = element_text(size = 18), 
      plot.title = element_text(size = 26), 
      plot.subtitle = element_text(size = 20), 
      legend.key.height = unit(2, "cm"),
     legend.key.width = unit(2, "cm"), 
     legend.text = element_text(size = 10),
     legend.title = element_text(size = 12)) + 
    scale_fill_manual(values = wesanderson::wes_palette("Darjeeling2", n = 4)) + xlab("Usage Period") + ylab("Percentage")



data%>%ggplot(mapping  = aes(x = Plan_Type, fill = Subscription)) + 
  geom_bar(position = "fill") + theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18), 
        plot.title = element_text(size = 26), 
        plot.subtitle = element_text(size = 20), 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm"), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling2", n = 4)) + xlab("Usage Period") + ylab("Percentage")


require(tidymodels)
data <- data%>%select(!c("Subscription_Willingness")) 

fastDummies::dummy_columns(data, 
                           select_columns = colnames(data)[!colnames(data) %in% "Target"],
                           remove_selected_columns = TRUE) -> data_dummy


#Split

initial_split(data_dummy, prop = 0.75, strata = "Target") -> split.object
training(split.object) -> train
testing(split.object) -> test



##Folds
vfold_cv(train, v = 5, repeats = 3, strata = "Target") -> train_folds


## Initial Model
#xgb <- boost_tree()%>%
#  set_engine("xgboost")%>%
#  set_mode("classification")
#rec_xgb <- recipe(Target~., data_dummy)
#wflow_xgb <- workflow()%>%add_model(xgb)%>%add_recipe(rec_xgb)
#fit(wflow_xgb, train) -> xgb_fit
#caret::confusionMatrix(predict(xgb_fit, test)$.pred_class, test$Target)


#xgb_tune <- boost_tree(trees = tune(),
#tree_depth = tune(),
#learn_rate = tune())%>%set_engine("xgboost")%>%set_mode("classification")

#xgb_tune%>%extract_parameter_set_dials()%>%
#grid_regular(levels = 3) -> xgb_grid
#wflow_xgb_tune <- workflow()%>%add_recipe(rec_xgb)%>%add_model(xgb_tune)



#wflow_xgb_tune%>%tune_grid(train_folds,grid = xgb_grid, 
#metrics = metric_set(roc_auc)) -> xgb_res

##


#xgb_res%>%collect_metrics() -> xgb_metrics

#xgb_metrics%>%filter(mean > 0.90)%>%
#pivot_longer(cols = colnames(xgb_metrics[,!colnames(xgb_metrics) %in% c("mean",".config",".metric",".estimator","std_err")]),
#values_to = "Value", names_to = "Spec")%>%ggplot(mapping = aes(y = Value, x = mean, color = .config)) + 
#geom_point(size = 6) + 
#facet_wrap(~Spec, scale = "free") + 
#theme_minimal() + ggtitle("Model Performance") + xlab("ROC-AUC") + ylab("Parameter Value")





#autoplot(xgb_res) + theme(legend.position = "top") 



xgb_param <- 
  tibble(
    trees = 1000,
    tree_depth = 15,
    learn_rate = 0.316
  )

xgb_tune <- boost_tree(trees = tune(),
                       tree_depth = tune(),
                       learn_rate = tune())%>%set_engine("xgboost")%>%set_mode("classification")

wflow_xgb_tune <- workflow()%>%add_recipe(rec_xgb)%>%add_model(xgb_tune)

wflow_xgb_tune%>%finalize_workflow(xgb_param) -> xgb_fin

fit(xgb_fin, train) -> xgb_fit_fin
caret::confusionMatrix(predict(xgb_fit_fin, test)$.pred_class, test$Target)


