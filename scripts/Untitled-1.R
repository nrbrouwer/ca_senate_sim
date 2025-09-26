  party_df <- data.frame(table(votes_including_s$`Party Aligned`))
  names(party_df) <- c('alignment', 'count')
  party_df$percentage <- round(party_df$count / sum(party_df$count) * 100, 1)
  pie_chart <- ggplot(party_df, aes(x = '', y = count, fill = alignment)) +
    geom_bar(stat = 'identity', width = 1) +
    coord_polar('y', start = 0) +
    theme_void() +
    labs(title = 'Party Alignment Breakdown') +
    geom_text(aes(label = paste0(percentage, '%')),
             position = position_stack(vjust = 0.5))
 print(pie_chart)
