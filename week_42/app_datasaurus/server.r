#runApp("D:/app_datasaurus")

function(input, output) {
	chosen_data <- reactive({
		chosen_data <- datasaurus[which(datasaurus$dataset == input$dataset),]
		chosen_data
	})
	posit <- reactive({
		input$posit
	})
	disp <- reactive({
		input$disp
	})
	output$posit <- renderTable({
		t_posit <- posi_table[which(posi_table$type %in% posit() & posi_table$dataset == input$dataset),]
		t_posit <- spread(t_posit[,-which(colnames(t_posit) == "dataset")],type,value)
		colnames(t_posit) <- gsub("param","",colnames(t_posit))
		t_posit
	})
	output$disp <- renderTable({
		t_disp <- disp_table[which(disp_table$type %in% disp() & disp_table$dataset == input$dataset),]
		t_disp <- spread(t_disp[,-which(colnames(t_disp) == "dataset")],type,value)
		colnames(t_disp) <- gsub("param","",colnames(t_disp))
		t_disp
	})
	

	output$plot_xy <- renderPlot({
		g <- ggplot(chosen_data(), aes(x=x, y=y, colour=dataset))
		g <- g + geom_point(col = "red", size = 3)
#		g <- g + theme_void()
		g <- g + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 10, hjust = 0.5, vjust = 0.5))
		g <- g + ggtitle(input$dataset)
		g <- g + xlab("x")
		g <- g + ylab("y")
		g <- g + labs(caption = "(For tidy tuesday, based on data from Alberto Cairo courtesy of Steph Locke + Lucy McGowan)")
		g <- ggMarginal(g,type = "histogram", xparams = list(fill = "green"), yparams = list(fill = "blue"))
 		print(g)
	})
}
