library(dash)
library(dashCoreComponents)
library(lubridate)
library(openxlsx)

file_reader = function(arq,coldate,sheet,test=FALSE){
  #arq='C:/Users/joao.santana/Desktop/trucks.xlsx'
  #coldate = 'Data'
  # Carregando arquivo
  data = openxlsx::read.xlsx(here::here('data', arq),sheet=sheet)
  # Transformando data
  names(data)[1] = 'Data'
  data[coldate]= as.Date(data[,'Data'],origin = '1899-12-30')
  if (test == TRUE){
    best_columns=c(
      "Data",
      "Trucks",
      "Trucks_m12",
      "IBC-BR",
      "Industrial.Production",
      "Commodity.Price.Index",
      "USD/BRL",
      "Base.Interest.Rate"
    )
  }
  else{
    best_columns=c(
      "Data",
      "Trucks_m12",
      "IBC-BR",
      "Industrial.Production",
      "Commodity.Price.Index",
      "USD/BRL",
      "Base.Interest.Rate"
    )
  }
  
  data = data[,best_columns]
  
  return(data)
}

df_prev=file_reader(arq='trucks_simple.xlsx',
                    coldate='Data',sheet=2)

#m = xgboost::xgb.load(here::here('data', 'trucks_xgb'))
#imp_matrix = xgboost::xgb.importance(feature_names = feat, model = m)

feat = c("Trucks_m12","`IBC-BR`","Industrial.Production","Commodity.Price.Index","`USD/BRL`","Base.Interest.Rate")
imp_matrix = readRDS(here::here('data', 'imp_matrix.rds'))

df_orig= file_reader(arq='trucks_simple.xlsx',
                     coldate='Data',sheet=1,test=T)

#previsto_12m = predict(m,as.matrix(df_prev[,-1]))

#eixo_x =append(df_orig$Data,df_prev$Data)
#eixo_y_pred = append(round(predict(m,as.matrix(df_orig[,-(1:2)])),0),rep(NA,12))
#eixo_y_obs = append(as.matrix(df_orig[,2]),rep(NA,12))
#eixo_y_12m = append(rep(NA,length(as.matrix(df_orig[,2]))),round(predict(m,as.matrix(df_prev[,-1])),0))


imports = c('previsto_12m','eixo_x','eixo_y_pred','eixo_y_obs','eixo_y_12m')

for(el in imports){
    assign(el,readRDS(here::here('data', paste0(el,'.rds'))))
}

#saveRDS(previsto_12m,'C:/dash_deploy/dashr-heroku-deployment-demo-main/data/previsto_12m.rds')
#saveRDS(eixo_x,'C:/dash_deploy/dashr-heroku-deployment-demo-main/data/eixo_x.rds')
#saveRDS(eixo_y_pred,'C:/dash_deploy/dashr-heroku-deployment-demo-main/data/eixo_y_pred')
#saveRDS(eixo_y_obs,'C:/dash_deploy/dashr-heroku-deployment-demo-main/data/eixo_y_obs.rds')
#saveRDS(eixo_y_12m,'C:/dash_deploy/dashr-heroku-deployment-demo-main/data/eixo_y_12m.rds')

plot(eixo_x,eixo_y_pred,type='l',col='red',
     main = 'Vendas de Caminhões',xlab = NA,ylab=NA)
lines(eixo_x,eixo_y_obs,type='l', col ='black')
lines(eixo_x,eixo_y_12m,type='l', col ='blue')
legend(x=eixo_x[1],y=max(eixo_y_obs,na.rm=T),
       legend=c('Observado',"Previsto","Projeção 12m"),
       col = c('black','red','blue'),
       lty = 1, merge = TRUE)   #, trace = TRUE)
texto = 'Projeções e predições por modelo XGBoost'
mtext(texto, side = 1, line = 3, cex = 0.8, adj = 1) 


#####
# Parâmetros Gr?ficos


############
#### Gerando Gráfico DCC 1

fig_g1 = list(
  data = list(
    list(
      x = eixo_x,
      y = eixo_y_obs,
      type = 'line',
      name = 'Observado'
    ),
    list(
      x = eixo_x,
      y = eixo_y_pred,
      type = 'line',
      name = 'Previsto'
    ),
    list(
      x = eixo_x,
      y = eixo_y_12m,
      type = 'line',
      name = 'Projeção 12m'
    )
  ),
  layout = list(title = list(text='Vendas de Caminhões - Série Histórica',
                             x=0.05,size=30,color='black'))
)

max(c(max(imp_matrix[,2]),max(imp_matrix[,3]),max(imp_matrix[,4])))*1.15

imp_matrix$Feature

############
#### Gerando Gr?fico DCC Importancia das Variaveis

fig_g2 = list(
  data = list(
    list(
      y = imp_matrix$Gain,
      x = imp_matrix$Feature,
      type = 'bar',
      name = 'Ganho'
    ),
    list(
      y = imp_matrix$Cover,
      x = imp_matrix$Feature,
      type = 'bar',
      name = 'Cobertura'
    ),
    list(
      y = imp_matrix$Frequency,
      x = imp_matrix$Feature,
      type = 'bar',
      name = 'Frequencia'
    )
  ),
  layout = list(title = list(text='Importância das Features do Modelo',
                             x=0.05,size=30,color='black'))
)




####
# GERANDO PAR?METROS CONTROLES


markers_list = list()
for (z in (which(lubridate::month(eixo_x) == 1))){
  markers_list[[z]]=lubridate::year(eixo_x[z])
}



available_indicators = c("Trucks_m12","IBC-BR","Industrial.Production","Commodity.Price.Index","USD/BRL","Base.Interest.Rate")
label = c('Trucks t-12', 'IBC-BR', "Industrial Production", "Commodity Price",
          "USD/BRL","Base Interest Rate")

option_indicator <- lapply(
  available_indicators,
  function(available_indicator) {
    list(label = available_indicator,
         value = available_indicator)
  }
)

for(x in seq(1,length(label))){
option_indicator[[x]][['label']] = label[x]
}


####
# CSS

style_top = list(
  'display'= 'grid',
  'grid-template-columns'= 'auto auto auto auto',
  'grid-gap'= '0px',
  'background-color' = '#425a7c',
  'padding'= '10px 10px 0px 10px',
  'height' =  '12vh'
)

style_top_boxes= list(
  'font-size'= '30px',
  'text-align'= 'center',
  'background-color' = '#425a7c',
  'padding'= '20px 50px 20px 50px',
  'width'='25vw'
)

style_grid= list(
  'display'= 'grid',
  'grid-template-columns'= 'auto auto auto auto',
  'grid-gap'= '0px',
  'background-color' = 'white',
  'padding'= '10px',
  'height' =  '84vh',
  'box-shadow' = 'inset 0 0 0 10px #f6f6f6'
)

style_left_box= list(
  'font-size'= '30px',
  'text-align'= 'center',
  'background-color' = '#425a7c',
  'padding'= '20px',
  'width'='15vw'
)

style_boxes= list(
  'font-size'= '30px',
  'text-align'= 'center',
  'background-color' = 'rgb(246 246 246)',
  'padding'= '0px 40px 8px 50px',
  'width'='55vw'
)


style_right_boxes= list(
  'font-size'= '30px',
  'text-align'= 'center',
  'background-color' = 'rgb(246 246 246)',
  'padding'= '0px 50px 20px 10px',
  'width'='33.8vw'
)

###

app = dash::dash_app(title='Vendas de Caminhoes')

app %>% set_layout(
  
  # TOP CONTROLS
  html$div(
    # FIRST TOP BOX
    html$div(
      html$h1('Vendas de Caminhões',
              style=list('color' =  'white', 'text-align'= 'left', 
                         'font-size'= '3vw', 'margin-top'= '0px',
                         'margin-bottom' = '0px','padding-left' = '3vw',
                         'padding-top'='1vw'))
    )
    # END TOP CONTROLS
    
    # SECOND TOP BOX
    
    # END SECOND TOP BOX
    
    ,style=style_top
  ),
  
  
  # MAIN LAYOUT
  html$div(
    
    
    # FIRST MAIN BOX
    html$div(
      
      #Espaçamento entre o topo
      
      html$div(id='divao',
               #SLIDER 1
               html$div(html$h4('Anos',style=list('margin' = '0px',
                                                  'font-size'='18px',
                                                  'text-align'='left',
                                                  'margin-bottom'='0vh')),
                        
                        html$div(
                        dccRangeSlider(
                          id = 'year-slider',
                          min = as.numeric(min(unique(lubridate::year(eixo_x)))),
                          max = as.numeric(max(unique(lubridate::year(eixo_x)))),
                          marks = setNames(as.list(as.character(unique(lubridate::year(eixo_x))))[seq(1,length(unique(lubridate::year(eixo_x))),2)],
                                           as.character(unique(lubridate::year(eixo_x)))[seq(1,length(unique(lubridate::year(eixo_x))),2)]),
                          value = c(2020,2022)),style = list('margin-top'='0.3vw')
                        )
                        
                        ,style=list('display'= 'grid',
                                    'grid-template-columns'= '3vw auto',
                                    'grid-gap'= '0px','margin-left'='3vw')
               ),
               # FIM SLIDER 1
               
               # CONTAINER SEPARADOR
               html$div(),
               # FIM CONTAINER SEPARADOR
               
               # SLIDER 2
               html$div(id='divao2',html$h4('Meses',style=list('margin' = '0px',
                                                               'font-size'='18px',
                                                               'text-align'='left',
                                                               'margin-bottom'='2vh')),
                        
                        html$div(
                        dccRangeSlider(
                          id = 'month-slider',
                          min = as.numeric(min(unique(lubridate::month(eixo_x)))),
                          max = as.numeric(max(unique(lubridate::month(eixo_x)))),
                          marks = setNames(as.list(as.character(unique(lubridate::month(eixo_x)))), as.character(unique(lubridate::month(eixo_x)))),
                          value = c(1,12)
                        ), style = list('margin-top'='0.3vw')
                        )
                        ,style=list('display'= 'grid',
                                    'grid-template-columns'= '3vw auto',
                                    'grid-gap'= '0px','margin-right'='3vw')
               )
               
               ,style= list('display'= 'grid',
                            'grid-template-columns'= 'auto 7vw auto',
                            'grid-gap'= '0px',
                            'margin-bottom' = '0vh', 'height' = '3vw','margin-top'='0.5vw')),
      
      #GRÁFICO
      dashCoreComponents::dccGraph(id='gone',style=list('width'='55vw','height'='35vw'),
      )
      #FIM GRÁFICO
      
      

      # FIM SLIDER 2
      
      
      ,style=style_boxes),
    # FINISH FIRST MAIN BOX
    
    # SECOND MAIN BOX
    html$div(id='test',
          html$div(
            
            
            html$div(),
            
            html$h4('Feature',style=list('margin-top' = '0.8vh',
                                          'font-size'='18px',
                                          'text-align'='left',
                                          'margin-bottom'='0.5vh',
                                          'margin-left'='0.5vw')),
            
            dccDropdown(
              id = 'graph_indicator',
              options = option_indicator,
              value = 'USD/BRL',style= list('width' = '10vw','height'='1vw',
                                            'font-size'='18px')
            ),

          style= list('display'= 'grid',
                      'grid-template-columns'= '19vw 5vw 6vw',
                      'grid-gap'= '0px',
                      'margin-bottom' = '5vh', 'height' = '0.64vw','margin-top'='0.44vw')),
          dashCoreComponents::dccGraph(id='gtwo',style=list('width'='34vw','height'='35vh'),
          ),
          html$div(style=list('height'='1vw')),
          dashCoreComponents::dccGraph(id='gthree',
                                       figure = fig_g2,style=list('width'='34vw','height'='35vh'),
          )
      ,style=style_right_boxes
    )
    # FINISH SECOND MAIN BOX
    
    
    ,style=style_grid
    
  )
  
)

# Adicionando interações
# grafico principal
app %>% add_callback(
  output('gone','figure'),
  list(
  input(id = 'year-slider', property = 'value'),
  input(id = 'month-slider', property = 'value')),
  function(input_value,input_value_month){
    
    df = cbind.data.frame(eixo_x,eixo_y_obs,eixo_y_pred,eixo_y_12m)
    
    filtro = (year(df$eixo_x) >= input_value[1] & year(df$eixo_x) <= input_value[2] &
      month(df$eixo_x) >= input_value_month[1] &
      month(df$eixo_x) <= input_value_month[2])
    
    figure = list(
      data = list(
        list(
          x = eixo_x[filtro],
          y = eixo_y_obs[filtro],
          type = 'line',
          name = 'Observado'
        ),
        list(
          x = eixo_x[filtro],
          y = eixo_y_pred[filtro],
          type = 'line',
          name = 'Previsto'
        ),
        list(
          x = eixo_x[filtro],
          y = eixo_y_12m[filtro],
          type = 'line',
          name = 'Projeção 12m'
        )
      ),
      layout = list(title = list(text='Vendas de Caminhões - Série Histórica',
                                 x=0.05,size=30,color='black'))
    )
    
    figure
  }
)

# gráfico features
app %>% add_callback(
  output('gtwo','figure'),
  list(
    input(id = 'year-slider', property = 'value'),
    input(id = 'month-slider', property = 'value'),
    input(id= 'graph_indicator', property = 'value')),
  function(input_value,input_value_month,indicator){
    
    df_prev2= df_prev
    df_prev2$Trucks = NA
    df_prev2 = df_prev2[,c(1,8,2:7)]
    
    df = rbind.data.frame(df_orig,df_prev2)
    df = df[,colnames(df) != 'Trucks']
    
    eixo_x = df$Data
    eixo_y = df[,colnames(df) == indicator]
    
    df = data.frame(eixo_x,eixo_y)
    
    filtro = (year(df$eixo_x) >= input_value[1] & year(df$eixo_x) <= input_value[2] &
                month(df$eixo_x) >= input_value_month[1] &
                month(df$eixo_x) <= input_value_month[2])
    
    figure = list(
      data = list(
        list(
          x = eixo_x[filtro],
          y = eixo_y[filtro],
          type = 'line',
          name = indicator
        )
      ),
      layout = list(title = list(text=ifelse(indicator != 'Trucks_m12',paste0(indicator, ' (t-12)'),'Trucks (t-12)'),
                                 x=0.05,size=30,color='black'))
    )
    
    figure
  }
)

app$run_server()
