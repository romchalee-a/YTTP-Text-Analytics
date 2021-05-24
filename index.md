Youth To The People - Text Analytics | R 

**Part 1 Business Insight Report** 

**Introduction**

This analysis has a goal to analyze the review comments of Superfood Antioxidant Cleanser by Youth To The People (YTTP) along with its competitors’ which are The Rice Wash Skin-Softening Cleanser by Tatcha and Soy Makeup Removing Face Wash by Fresh as well as to explore business insights. Each review dataset is consisting of 60 customer reviews collected from the Sephora website and each review dataset was imported, tokenized, and removed stop words before conducting text analytics (shown in Code 1,2, and 3). 
 
**Framework 1: token frequency histograms**

The most common word in YTTP customer reviews are including skin, love, dry, bottle, acne, oily, feel/feeling, clean, wash, time, combination, worth, smell/smells, drying, amazing, size, pump, nice, and leaves, winter, normal, and glass (shown in Output 1). From these results, we can hypothesize that customers mentioned skin types or problems (oily, normal, dry, combination), packaging (glass bottle, pump bottle), price worthy, and product experience (smell, clean, wash, time). 

![Output 1: YTTP Review Word Counts](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture1.png)

The most common word in Fresh customer reviews are including skin, love, gentle, clean, wash, fresh, makeup, dry, feeling, sensitive, acne, soft, smell, nice, doesn’t, scent, smell, and amazing  (shown in Output 2). From these results, we can hypothesize that customers mentioned skin types or problems (dry, sensitive), product experience (scent/smell, makeup, acne), and feeling after using the product (fresh, nice, soft, amazing). Another common word that should be considered is “doesn’t”, it might refer to either negative or positive meaning when combined with words. 

![Output 2: Fresh Review Word Counts](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture2.png)
 
The most common word in Tatcha customer reviews are including skin, love, dry, wash, feel, sensitive, gentle, clean, nice, soft, cream, scent, hydrated, makeup, fresh, lather, and don’t (shown in Output 3). From these results, we can hypothesize that customers mentioned skin types or problems (sensitive), product experience (scent, cream, lather, gentle, makeup), and feeling after using the product (clean, soft, fresh, hydrated). Another common word that should be considered is “don’t”, it might refer to either negative or positive meaning when combined with words. 

![Output 3: Tatcha Review Word Counts](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture3.png)
 
**Framework 2: correlograms**

From the correlograms shown in Output 4, “acne” appeared in the comments with similar frequency across YTTP, Fresh, and Tatcha reviews. This insight can be hypothesized that when customers using cleansers they are commonly paying attention to acne issues or product effects on acne issues. 

![Output 4: correlograms](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture4.png)

**Framework 3: correlation test**

According to the correlations of YTTP and Fresh and YTTP and Tatch, both correlations are slightly different (0.9847701, 0.9991659) and both have a high positive correlation with YTTP word frequency (shown in Output 5 and 6). This means that the comments of the three brands are mentioning similar topics.  

![Output 5: YTTP and Fresh correlation test](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture5.png)
![Output 6: YTTP and Tatch](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture6.png)

**Framework 4: sentiment analysis**

After conducting sentiment analysis with the YTTP review shown in Output 7, we can observe that love is the most frequent word following by clean, worth, amazing, and nice. These words are appeared more than 10 times and have higher frequencies and portion compared to the negative words. Furthermore, some of the negative words do not refer to negative meaning considered with the review context. For example, smell refers to the smell of the cleanser product and cheap refers to the price of the product. From this, we can conclude that YTTP reviews tend to have more positive reviews than negative reviews. And those positive reviews are frequently about product compliments. 

![Output 7: the contribution to positive and negative sentiment in YTTP review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture7.png)
 
Similar to YTTP’s top positive word, “love” is also the highest frequency of positive words for Fresh’s and Tatcha’s reviews shown in Output 8 and 9. The positive words for these three brands are also similar to each other. Most of them can be assumed as a compliment word, for example, recommend, amazing, and nice.  we can also observe a have higher frequencies and portion of positive words compared to negative words same as YTTP.  Therefore, we can conclude that both Fresh and Tatcha reviews tend to have more positive reviews than negative reviews.

![Output 8: the contribution to positive and negative sentiment in Fresh review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture8.png)
![Output 9: the contribution to positive and negative sentiment in Tatcha review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture9.png)
 
To support this interpretation, these three products are ranked between 4-4.3 stars on the Sephora website shown in Figures 1,2, and 3. 

![Output 10: bigram graph representing paired word in YTTP review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture10.png)

**Framework 5: bigram graph**

Since we can create hypotheses by looking at frequency histogram from Framework 1, now we are investigating more deeply into words that usually appear together.  
 
For YTTP, “Skin” usually appears with so many words, for instance, dry skin, normal skin, prone skin, sensitive skin, combination skin, and oily skin which aligned with the hypothesis that customers ’ mentioning their skin condition in the reviews (shown in Output 10). From this insight, we can assume that the customer might review the product based on their skin type. Moreover, there are paired words that seem to generate more insight into customer’s focusing topics including product ingredient: vegan product, fragrance-free, packaging: glass bottle, compliment: highly recommend, acne issue: hormonal acne 
 
For Fresh, “skin” is also the most common word paired with other words same as YTTP. But there is one interesting paired word,  “skin barrier” which might refer to how the product affects the skin barrier. Another area of customer review context is the cleanser efficiency which can be observed through the following pair word: “remove makeup”, “double cleansing”, “gentle cleanser”, and “super gentle” (shown in Output 11).  

![Output 11: bigram graph representing paired word in Fresh review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture11.png)
 
For Tatcha,  “skin” is also the most common word paired with other words same as Fresh and YTTP. The insights we can observe with the bigram graph shown in Output 12 are product experience that shown in the “doesn’t” pair which are “doesn’t strip” and “doesn’t irritate”. These pair are resenting a positive review along with the pair of “highly recommend”. Furthermore, several pairs are also describing customer’s feeling while using a product, for instance, “nice lather”, “scent light”, and “gentle cleanser”. For Tatcha review, it also has a word pair related to acne same as Fresh and YTTP, that pair is “prone acne”. 

![Output 12: bigram graph representing paired word in Tatcha review](https://github.com/romchalee-a/YTTP-Text-Analytics/blob/main/image_yttp/Picture12.png)

**Conclusion**  
The topics that all three brand reviews share similarity are how the product suits customer’s skin types and problems, customer’s experience, and how the customers feel about their skin after applying the product. Furthermore, all of the three brands seem to have a positive review than a negative review.  
 
With the hypotheses across the three brands from Framework 1 and the result from Framework 5, we can say that product packing and product price are both distinctive topics mentioned in the YTTP review which we are not frequently observed in the other competitor’s reviews.  
 
YTTP offers a sustainable packaging material that is glass and reusable (What sustainability means to us 2019). This showed that the YTTP sustainability approach on sustainable and reusable packing (glass bottle) is successfully created a competitive advantage and received attention from customers. While Fresh cleanser reviews seem to be distinctive with the ability to remove makeup and double-cleansing. Tatcha reviews seem to be distinctive with non-strip and irritation effects. 

**Part 2: Coding**

**Import Data**
```
Code 1 
###################################################### 
# import doc file and create text object 
###################################################### library(textreadr) library(textshape) 
# fresh 
setwd("/Users/admin/Desktop/Text_Analytics/Assignment_3/fresh") 
fresh <- list.files(path="/Users/admin/Desktop/Text_Analytics/Assignment_3/fresh") fresh_text <- do.call(rbind, lapply(fresh, function(x) read_document(file=x)))  
 
# tatcha 
setwd("/Users/admin/Desktop/Text_Analytics/Assignment_3/tatcha") 
tatcha <- list.files(path="/Users/admin/Desktop/Text_Analytics/Assignment_3/tatcha") tatcha_text <- do.call(rbind, lapply(tatcha, function(x) read_document(file=x)))  
 
# youth to the people 
setwd("//Users/admin/Desktop/Text_Analytics/Assignment_3/youth to the people") yttp <- list.files(path="//Users/admin/Desktop/Text_Analytics/Assignment_3/youth to the people") yttp_text <- do.call(rbind, lapply(yttp, function(x) read_document(file=x))) 
 
Code 2 
###################################################### 
# create a data frame and turn text into lower case 
###################################################### fresh_review <- data.frame(line=1:60 , text=fresh_text) tatcha_review <- data.frame(line=1:60 , text=tatcha_text) yttp_review <- data.frame(line=1:60 , text=yttp_text) 
 
fresh_review$text <- tolower(fresh_review$text) tatcha_review$text <- tolower(tatcha_review$text) yttp_review$text <- tolower(yttp_review$text) 
 
Code 3 
###################################################### 
# tokenizing, and removing stop word 
###################################################### library(tidytext) library(tidyverse) library(stringr) data("stop_words") # create custom stop words custom_stop_words <- tribble(     ~word,  ~lexicon, 
    "product", "CUSTOM",     "cleanser","CUSTOM", 
    "it’s", "CUSTOM", 
    "i’ve", "CUSTOM", 
    "i’m","CUSTOM", 
) 
stop_words2 <- stop_words %>%  
    bind_rows(custom_stop_words) 
 
fresh_token_nostop <- fresh_review %>%     unnest_tokens(word, text) %>%     anti_join(stop_words2) %>%      count(word, sort=TRUE) 
 
tatcha_token_nostop <- tatcha_review %>% 
unnest_tokens(word, text) %>% 
    anti_join(stop_words2) %>%  
    count(word, sort=TRUE) 
 
yttp_token_nostop <- yttp_review %>%     unnest_tokens(word, text) %>%     anti_join(stop_words2) %>%  
    count(word, sort=TRUE) 
 
print(fresh_token_nostop) print(tatcha_token_nostop) print(yttp_token_nostop) 

```
**Framework1: token frequency histograms**
```
####################################################### 
# Framework1: token frequency histograms 
#######################################################
library(ggplot2) 
yttp_hist <- yttp_token_nostop %>%     filter(n>6) %>%  
    mutate(word = reorder(word,n )) %>% 
    ggplot(aes(word, n))+     geom_col()+     xlab(NULL)+     coord_flip()+ 
    ggtitle("YTTP Review Word Counts") 
 
fresh_hist <- fresh_token_nostop %>% 
    filter(n>6) %>%  
    mutate(word = reorder(word,n )) %>% 
    ggplot(aes(word, n))+     geom_col()+     xlab(NULL)+     coord_flip()+ 
    ggtitle("Fresh Review Word Counts") 
 
tatcha_hist <- tatcha_token_nostop %>%     filter(n>6) %>%  
    mutate(word = reorder(word,n )) %>% 
    ggplot(aes(word, n))+     geom_col()+     xlab(NULL)+     coord_flip()+ 
    ggtitle("Tatcha Review Word Counts") 
```
**Framework2: correlograms**
```
############################################# 
# Framework2: correlograms 
############################################# library(tidyr) 
frequency <- bind_rows(mutate(fresh_token_nostop, brand="Fresh"),                        mutate(tatcha_token_nostop, brand= "Tatcha"),                        mutate(yttp_token_nostop, brand="YTTP") 
)%>%#closing bind_rows (combine all tgt) 
    mutate(word=str_extract(word, "[a-z']+")) %>% # order: a-z     count(brand, word) %>%  
    group_by(brand) %>% # groupby location info 
    mutate(proportion = n/sum(n))%>% # create new col -> proportion     select(-n) %>%  #remove n coz we focus on proportion     spread(brand, proportion) %>% 
gather(brand, proportion, `Tatcha`, `Fresh`) 
 
# plot the correlograms: 
library(scales) 
ggplot(frequency, aes(x=proportion, y=`YTTP`,                        color = abs(`YTTP`- proportion)))+     geom_abline(color="grey40", lty=2)+ 
    geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+     geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +     scale_x_log10(labels = percent_format())+     scale_y_log10(labels= percent_format())+ 
    scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+     facet_wrap(~brand, ncol=2)+     theme(legend.position = "none")+ 
    labs(y= "YTTP", x=NULL) 
```
**Framework3: correlation test**
```
########################################## 
# Framework3: correlation test 
########################################## cor.test(data=frequency[frequency$brand == "Fresh",], 
         ~proportion + `YTTP`) 
 
cor.test(data=frequency[frequency$brand == "Tatcha",], 
         ~proportion + `YTTP`) 
```

**Framework4: sentiment analysis**
```
####################################################### 
# Framework4: sentiment analysis       
####################################################### 
token_fresh <- fresh_review %>%     unnest_tokens(word, text) print(token_fresh) 
 
token_tatcha <- tatcha_review %>%     unnest_tokens(word, text) print(token_tatcha) 
 
token_yttp <- yttp_review %>%     unnest_tokens(word, text) 
print(token_yttp) 
 
############################ 
# YTTP 
############################ 
# negative positive text library(tidytext) 
bing_yttp_word_count <- token_yttp %>% 
anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>%     count(word, sentiment, sort = TRUE) %>%  
    ungroup() 
 
bing_yttp_word_count_hist <-  bing_yttp_word_count %>%     group_by(sentiment) %>%      top_n(15) %>%      ungroup() %>%      mutate(word = reorder(word, n)) %>%  
    ggplot(aes(word, n, fill = sentiment))+     geom_col(show.legend = FALSE)+     facet_wrap(~sentiment, scales = 'free_y')+     labs(y = 'YTTP Sentiment Analysis (bing)',          x = NULL )+     coord_flip() 
print(bing_yttp_word_count_hist) 

############################ 
# Fresh 
############################ 
# negative positive text 
library(tidytext) 
bing_fresh_word_count <- token_fresh %>% 
    anti_join(stop_words) %>%     inner_join(get_sentiments("bing")) %>%     count(word, sentiment, sort = TRUE) %>%  
    ungroup() 
 
bing_fresh_word_count_hist <-  bing_fresh_word_count %>%     group_by(sentiment) %>%      top_n(15) %>%  
    ungroup() %>%  
    mutate(word = reorder(word, n)) %>%  
    ggplot(aes(word, n, fill = sentiment))+     geom_col(show.legend = FALSE)+     facet_wrap(~sentiment, scales = 'free_y')+     labs(y = 'Fresh Sentiment Analysis (bing)',          x = NULL )+ 
    coord_flip() 
print(bing_fresh_word_count_hist) 

############################ 
# Tatcha 
############################ 
# negative positive text library(tidytext) 
bing_tatcha_word_count <- token_tatcha %>%     anti_join(stop_words) %>%     inner_join(get_sentiments("bing")) %>%     count(word, sentiment, sort = TRUE) %>%      ungroup() 
 
bing_tatcha_word_count_hist <-  bing_tatcha_word_count %>%     group_by(sentiment) %>%      top_n(15) %>%      ungroup() %>%      mutate(word = reorder(word, n)) %>%      ggplot(aes(word, n, fill = sentiment))+     geom_col(show.legend = FALSE)+     facet_wrap(~sentiment, scales = 'free_y')+     labs(y = 'Tatcha Sentiment Analysis (bing)', 
         x = NULL )+     coord_flip() 
print(bing_tatcha_word_count_hist) 

```
**Framework5: bigram graph**
```
############################ 
# YTTP 
############################ 
# token bigrams 
yttp_bigrams <- yttp_review %>%  
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>%     filter(!is.na(bigram)) 
# remove case with stop word 
library(tidyr) 
yttp_bigrams_sep <- yttp_bigrams %>%      separate(bigram, c("word1", "word2"), sep = " ") yttp_bigrams_fil <- yttp_bigrams_sep %>%     filter(!word1 %in% stop_words$word) %>%     filter(!word2 %in% stop_words$word) #creating the new bigram, "no-stop-words": yttp_bigram_count <- yttp_bigrams_fil %>%     count(word1, word2, sort = TRUE) 
# plot ggraph library(igraph) library(ggraph) 
yttp_bigram_graph <- yttp_bigram_count  %>%     filter(n>1) %>% #try to adjust this number     graph_from_data_frame() ggraph(yttp_bigram_graph, layout = "fr") +     geom_edge_link()+     geom_node_point()+ 
    geom_node_text(aes(label=name), vjust =1, hjust=1) 

############################ 
# Fresh 
############################ 
# token bigrams 
fresh_bigrams <- fresh_review %>%  
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>%     filter(!is.na(bigram)) 
 
# remove case with stop word 
library(tidyr) 
fresh_bigrams_sep <- fresh_bigrams %>%  
    separate(bigram, c("word1", "word2"), sep = " ") 
 
fresh_bigrams_fil <- fresh_bigrams_sep %>%     filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) 
 
#creating the new bigram, "no-stop-words": fresh_bigram_count <- fresh_bigrams_fil %>% 
    count(word1, word2, sort = TRUE) 
 
# plot ggraph library(igraph) library(ggraph) fresh_bigram_graph <- fresh_bigram_count  %>%     filter(n>1) %>% #try to adjust this number     graph_from_data_frame() ggraph(fresh_bigram_graph, layout = "fr") +     geom_edge_link()+     geom_node_point()+ 
    geom_node_text(aes(label=name), vjust =1, hjust=1) 

############################ 
# Tatcha 
############################ 
# token bigrams 
tatcha_bigrams <- tatcha_review %>%  
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>%     filter(!is.na(bigram)) 
 
# remove case with stop word 
library(tidyr) 
tatcha_bigrams_sep <- tatcha_bigrams %>%      separate(bigram, c("word1", "word2"), sep = " ") tatcha_bigrams_fil <- tatcha_bigrams_sep %>%     filter(!word1 %in% stop_words$word) %>%     filter(!word2 %in% stop_words$word) #creating the new bigram, "no-stop-words": 
tatcha_bigram_count <- tatcha_bigrams_fil %>%     count(word1, word2, sort = TRUE) 
# plot ggraph library(igraph) library(ggraph) 
tatcha_bigram_graph <- tatcha_bigram_count  %>%     filter(n>1) %>% #try to adjust this number     graph_from_data_frame() ggraph(tatcha_bigram_graph, layout = "fr") +     geom_edge_link()+     geom_node_point()+ 
    geom_node_text(aes(label=name), vjust =1, hjust=1) 
   
```

------------------------------------------------------------------------------------------------------------------------------------------------------------
References 
 
What sustainability means to us. (2019, December 03). Retrieved February 10, 2021, from https://www.youthtothepeople.com/blogs/to-the-people/what-sustainability-means-to-us 
 
 
Source of dataset 
 
Superfood Antioxidant Cleanser by Youth To The People  
Superfood antioxidant cleanser - youth to the people. (n.d.). Retrieved February 10, 2021, from https://www.sephora.com/product/kale-spinach-green-tea-age-prevention-cleanser-P411387 
Soy Makeup Removing Face Wash by Fresh 
Soy makeup removing face wash - fresh. (n.d.). Retrieved February 10, 2021, from https://www.sephora.com/product/soy-face-cleanser-P7880 
The Rice Wash Skin-Softening Cleanser by Tatcha 
The rice wash skin-softening cleanser - tatcha. (n.d.). Retrieved February 10, 2021, from https://www.sephora.com/product/tatcha-the-rice-wash-skin-softening-cleanser-P461537 

