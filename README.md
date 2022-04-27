# Dashboard Venda de Caminhões

Esse dashboard tem como objetivo apresentar e modelar a venda de caminhões no período em questão afim de oferecer uma projeção para os próximos doze meses, foi utilizado para treinamento um modelo XGBOOST com 6 features, a metrica objetivo foi a Média Absoluta dos Erros (MAE), a profundidade máxima da árvore foi 5,
            o passo de aprendizagem (eta) foi 1, o número de rodadas foi 200, o lambda para regularização l1 foi de 0.001. A base de dados foi separada inicialmente em duas, uma para treino e outra para teste, durante a etapa de treinamento a amostra foi repartida em 80% para treino e 20% para validação, validação nesse caso seria a parcela definida para ajuste de hiperparâmetro para manter a natureza da amostra de treino de tentar se aproximar do resultado esperado em produção. Como a ideia era projetar para os próximos doze meses, as features foram utilizadas em t-12 para evitar qualquer tipo de condução do erro na projeção.
            
O link para o dashboard é https://trucksxgb.herokuapp.com/.

![image](https://user-images.githubusercontent.com/60692882/165460429-81be452a-8de9-4705-b492-784a2317ea7c.png)
