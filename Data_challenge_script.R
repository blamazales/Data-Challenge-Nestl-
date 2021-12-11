install.packages("tidyverse")
library(tidyverse)


###
#TABELA tb_addresses
#analisando tabela
str(tb_addresses) #estrutura do objeto
class(tb_addresses) #classe do objeto
glimpse(tb_addresses) #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_addresses <- select(tb_addresses, -ponto_referencia)
tb_addresses <- filter(tb_addresses, tb_addresses$id_fornecedor != "NA")
tb_addresses <- mutate_all(tb_addresses, ~replace(., is.na(.), "00000000")) #tudo que possui NA virou 0
  
###########################################
#TABELA tb_banners
#analisando tabela
str(tb_banners) #estrutura do objeto
class(tb_banners) #classe do objeto
glimpse(tb_banners)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
#nada foi feito nesta tabela

###########################################
#TABELA tb_cities
#analisando tabela
str(tb_cities) #estrutura do objeto
class(tb_cities) #classe do objeto
glimpse(tb_cities)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
#tabela ok

##############################################
#TABELA coupons
#analisando tabela
str(tb_coupons) #estrutura do objeto
class(tb_coupons) #classe do objeto
glimpse(tb_coupons)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
ajuste_data_fim <- str_sub(tb_coupons$data_fim, 1, 10)
ajuste_data_inicio <- str_sub(tb_coupons$data_inicio, 1, 10)

tb_coupons <- tb_coupons %>%  mutate(data_fim = ajuste_data_fim,
                                     data_inicio = ajuste_data_inicio)

tb_coupons <- filter(tb_coupons, tb_coupons$data_fim != "NA")
tb_coupons <- filter(tb_coupons, tb_coupons$data_inicio != "NA")

tb_coupons <- mutate_all(tb_coupons, ~replace(., is.na(.), "0")) #tudo que possui NA virou 0

################################################
#TABELA customer_payments
#analisando tabela
str(tb_customer_payments) #estrutura do objeto
class(tb_customer_payments) #classe do objeto
glimpse(tb_customer_payments)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
ajuste_data <- str_sub(tb_customer_payments$data_pagamento, 1, 10)
ajuste_hora <- str_sub(tb_customer_payments$data_pagamento, 12, 19)

tb_customer_payments <- tb_customer_payments %>%  mutate(data_pagamento = ajuste_data,
                                                         hora_pagamento = ajuste_hora)

#################################################

#TABELA ordered_products
str(tb_ordered_products) #estrutura do objeto
class(tb_ordered_products) #classe do objeto
glimpse(tb_ordered_products)  #estrutura objetos

#analisando tabela

##limpar dados, filtrar linhas com informações faltantes
tb_ordered_products <- filter(tb_ordered_products, tb_ordered_products$id_ordem != "NA")
tb_ordered_products <- mutate_all(tb_ordered_products, ~replace(., is.na(.), "00000000")) #tudo que possui NA virou 0
tb_ordered_products <- select(tb_ordered_products, -subtotal)

subtotal <- tb_ordered_products$preco * tb_ordered_products$qtde
tb_ordered_products <- tb_ordered_products %>%  mutate(subtotal = subtotal)

###############################################starts_with
#TABELA product_tags
#analisando tabela
str(tb_product_tags) #estrutura do objeto
class(tb_product_tags) #classe do objeto
glimpse(tb_product_tags)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes

#tabela ok

################################################
#TABELA products
#analisando tabela
str(tb_products) #estrutura do objeto
class(tb_products) #classe do objeto
glimpse(tb_products)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_products_2 <- tb_products
tb_products <- select(tb_products, -id_disconto)
tb_products <- select(tb_products, -arquivado)
tb_products <- select(tb_products, -recuperavel)
tb_products <- filter(tb_products, tb_products$id_tipo != "NA")
tb_products <- filter(tb_products, tb_products$ativo != "NA")
tb_products <- mutate_all(tb_products, ~replace(., is.na(.), 0))

tb_products <- mutate(tb_products, 
                      ativo = replace(ativo, ativo==1, "TRUE"),
                      ativo = replace(ativo, ativo==0, "FALSE"))
tb_products <- mutate(tb_products, 
                      aprovado = replace(aprovado, aprovado==1, "TRUE"),
                      aprovado = replace(aprovado, aprovado==0, "FALSE")) 
tb_products <- mutate(tb_products, 
                      entregavel = replace(entregavel, entregavel==1, "TRUE"),
                      entregavel = replace(entregavel, entregavel==0, "FALSE"))

###################################################
#TABELA ratings
#analisando tabela
str(tb_ratings) #estrutura do objeto
class(tb_ratings) #classe do objeto
glimpse(tb_ratings)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_ratings <- filter(tb_ratings, tb_ratings$comparecimento != "NA")
tb_ratings <- filter(tb_ratings, tb_ratings$sabor != "NA")
tb_ratings <- filter(tb_ratings, tb_ratings$aparencia != "NA")

#######################################################
#TABELA soldis_cognatis
#analisando tabela
str(tb_solds_cognatis) #estrutura do objeto
class(tb_solds_cognatis) #classe do objeto
glimpse(tb_solds_cognatis)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_solds_cognatis_2 <- tb_solds_cognatis 

#tirar ultimos 3 digitos do cep
ajuste_cep <- str_sub(tb_solds_cognatis$CEP, 1, 5)
tb_solds_cognatis <- tb_solds_cognatis %>%  mutate(CEP = ajuste_cep)
tb_solds_cognatis <- filter(tb_solds_cognatis, tb_solds_cognatis$SETOR_ATIV != "NA")

#######################################################
#TABELA users
#analisando tabela
str(tb_users) #estrutura do objeto
class(tb_users) #classe do objeto
glimpse(tb_users)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_users_2 <-tb_users
#tb_users <- tb_users_2
tb_users <- select(tb_users, -nome_completo)
#tb_users <- select(tb_users, -id_fornecedor)
tb_users <- select(tb_users, -data_ultima_compra)
tb_users <- filter(tb_users, tb_users$ticket_medio != "NA")
tb_users <- filter(tb_users, tb_users$id_fornecedor != "NA")

#######################################################
#TABELA vendors_payments
#analisando tabela
str(tb_vendor_payments) #estrutura do objeto
class(tb_vendor_payments) #classe do objeto
glimpse(tb_vendor_payments)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
#tabela ok

#####################################################
#TABELA vendors
#analisando tabela
str(tb_vendors) #estrutura do objeto
class(tb_vendors) #classe do objeto
glimpse(tb_vendors)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_vendors_2 <- tb_vendors
tb_vendors <- select(tb_vendors, -nome_empresa)
ajuste_data_1 <- str_sub(tb_vendors$data_aprovacao, 1, 10)
ajuste_hora_1 <- str_sub(tb_vendors$data_aprovacao, 12, 19)
ajuste_data_2 <- str_sub(tb_vendors$data_reprovacao, 1, 10)
ajuste_hora_2 <- str_sub(tb_vendors$data_reprovacao, 12, 19)
ajuste_dregistro <- str_sub(tb_vendors$data_registro, 1, 10)
ajuste_hregistro <- str_sub(tb_vendors$data_registro, 12, 19)


tb_vendors <- tb_vendors %>%  mutate(data_aprovacao = ajuste_data_1,
                                      hora_aprovacao = ajuste_hora_1,
                                     data_reprovacao = ajuste_data_2,
                                     hora_reprovacao = ajuste_hora_2,
                                     data_registro = ajuste_dregistro,
                                     hora_registro = ajuste_hregistro)


###################################################
#TABELA vendors_delivery_fees
#analisando tabela
str(tb_vendors_delivery_fees) #estrutura do objeto
class(tb_vendors_delivery_fees) #classe do objeto
glimpse(tb_vendors_delivery_fees)  #estrutura objetos

##limpar dados, filtrar linhas com informações faltantes
tb_vendors_delivery_fees <- mutate_all(tb_vendors_delivery_fees, ~replace(., is.na(.), 0.0))

#######################################################
#CONVERTER DADOS

#TABELA tb_coupons 

tb_coupons$limite_custo <- as.double(tb_coupons$limite_custo)
tb_coupons$valor_minimo <- as.double(tb_coupons$valor_minimo)
tb_coupons$valor_total_uso <- as.double(tb_coupons$valor_total_uso)
tb_coupons$percentagem <- as.double(tb_coupons$percentagem)

tb_coupons$dias_duracao <- as.integer(tb_coupons$dias_duracao)
tb_coupons$novos_clientes_adquiridos <- as.integer(tb_coupons$novos_clientes_adquiridos)
tb_coupons$limite_uso_por_cpf <- as.integer(tb_coupons$limite_uso_por_cpf)
tb_coupons$limite_uso <- as.integer(tb_coupons$limite_uso)
tb_coupons$qtde_uso <- as.integer(tb_coupons$qtde_uso)

tb_coupons$entrega_free <- as.logical(tb_coupons$entrega_free)

##################################################################
#TABELA tb_ordered_products
tb_ordered_products$valor_medida <- as.double(tb_ordered_products$valor_medida)
tb_ordered_products$preco <- as.double(tb_ordered_products$preco)
tb_ordered_products$qtde <- as.integer(tb_ordered_products$qtde)
tb_ordered_products$numero_pessoas_servidas <- as.integer(tb_ordered_products$numero_pessoas_servidas)

###############################################################
#TABELA tb_ratings
tb_ratings$comparecimento <- as.integer(tb_ratings$comparecimento)
tb_ratings$sabor <- as.integer(tb_ratings$sabor)
tb_ratings$aparencia <- as.integer(tb_ratings$aparencia)

tb_ratings$media_avaliacao <- round(tb_ratings$media_avaliacao, 2)

###################################################################
#tabela tb_solds_cognatis 
tb_solds_cognatis$TRABALHADORES <- as.integer(tb_solds_cognatis$TRABALHADORES)
tb_solds_cognatis$S_FLUXO_PEDESTRE <- as.character(tb_solds_cognatis$S_FLUXO_PEDESTRE)
tb_solds_cognatis$S_FLUXO_VEICULOS <- as.character(tb_solds_cognatis$S_FLUXO_VEICULOS)
tb_solds_cognatis$S_POP_TOTAL <- as.integer(tb_solds_cognatis$S_POP_TOTAL)
tb_solds_cognatis$S_DOM_PART_PERM <- as.integer(tb_solds_cognatis$S_DOM_PART_PERM)
tb_solds_cognatis$S_SEM_FUNC <- as.integer(tb_solds_cognatis$S_SEM_FUNC)
tb_solds_cognatis$S_DE_1_4_FUNC <- as.integer(tb_solds_cognatis$S_DE_1_4_FUNC)
tb_solds_cognatis$S_DE_5_9_FUNC <- as.integer(tb_solds_cognatis$S_DE_5_9_FUNC)
tb_solds_cognatis$S_DE_10_19_FUNC <- as.integer(tb_solds_cognatis$S_DE_10_19_FUNC)
tb_solds_cognatis$S_DE_20_49_FUNC <- as.integer(tb_solds_cognatis$S_DE_20_49_FUNC)
tb_solds_cognatis$S_DE_50_99_FUNC <- as.integer(tb_solds_cognatis$S_DE_50_99_FUNC)
tb_solds_cognatis$S_DE_100_249_FUNC <- as.integer(tb_solds_cognatis$S_DE_100_249_FUNC)
tb_solds_cognatis$S_DE_250_499_FUNC <- as.integer(tb_solds_cognatis$S_DE_250_499_FUNC)
tb_solds_cognatis$S_DE_500_999_FUNC <- as.integer(tb_solds_cognatis$S_DE_500_999_FUNC)
tb_solds_cognatis$S_MAIS1000_FUNC <- as.integer(tb_solds_cognatis$S_MAIS1000_FUNC)
tb_solds_cognatis$S_SUPERMERCADO <- as.integer(tb_solds_cognatis$S_SUPERMERCADO)
tb_solds_cognatis$S_TT_PADARIA <- as.integer(tb_solds_cognatis$S_TT_PADARIA)
tb_solds_cognatis$S_ATACADO <- as.integer(tb_solds_cognatis$S_ATACADO)
tb_solds_cognatis$S_HIPERMERCADO <- as.integer(tb_solds_cognatis$S_HIPERMERCADO)
tb_solds_cognatis$S_MINIMERCADO <- as.integer(tb_solds_cognatis$S_MINIMERCADO)

##########################################

#TABELA tb_users

tb_users$ultima_qtde_compra <- as.integer(tb_users$ultima_qtde_compra)

###########################################

#TABELA tb_vendors 
tb_vendors$qtde_produtos <- as.integer(tb_vendors$qtde_produtos)
tb_vendors$hora_abertura <- as.character(tb_vendors$hora_abertura)
tb_vendors$hora_fechamento <- as.character(tb_vendors$hora_fechamento)
tb_vendors$delivery_raio_km <- as.double(tb_vendors$delivery_raio_km)

################################################
#JOINs

#right_join_products <- right_join(tb_products, tb_ratings,
#                                by = "id_produto")
#right_join_products <- right_join(tb_products, tb_ordered_products,
#                                by = "id_produto")

left_products <- left_join(tb_products, tb_ratings,tb_ordered_products,
                                by = "id_produto")
left_products <- left_join(tb_products, tb_vendors,
                           by = "id_fornecedor")

join_vendors <- left_join(tb_vendors, tb_vendors_delivery_fees, tb_users,
                          by = "id_fornecedor")

##########################################
join_addresses <- left_join(tb_cities, tb_addresses,
                           by = c("cidade" ,"estado", "uf"))

join_vendors <- left_join(join_addresses, tb_vendors,
                                            by = c("id_fornecedor" ,"id_endereco"))

join_vendors <- select(join_vendors, -pais.y)
join_vendors <- select(join_vendors, -data_aprovacao)
join_vendors <- select(join_vendors, -data_reprovacao)
join_vendors <- select(join_vendors, -hora_aprovacao)
join_vendors <- select(join_vendors, -hora_reprovacao)
join_vendors <- select(join_vendors, -nome_empresa)


analise_endereco <- left_join(join_vendors, tb_vendor_payments,
                               by = "id_fornecedor")

##############################################################
join_users <- left_join(tb_users, tb_vendors,
                        by = "id_fornecedor")

join_users <- select(join_users,  -qtde_produtos)
join_users <- select(join_users,  -preco_medio)
join_users <- select(join_users,  -avaliacao_media)
join_users <- select(join_users,  -nome_empresa)

join_vendor_payments <- left_join(join_users, tb_vendor_payments,
                        by = "id_fornecedor")

analise_usuario <- left_join(join_vendor_payments, tb_addresses,
                             by = c("id_fornecedor", "id_endereco"))

join_users <- select(join_users,  -qtde_produtos)
join_users <- select(join_users,  -preco_medio)
join_users <- select(join_users,  -avaliacao_media)
join_users <- select(join_users,  -nome_empresa)

############################################################


analise_pagamento <- left_join(tb_users, tb_products,
                           by = "id_fornecedor")

analise_pagamento <- left_join(analise_pagamento, tb_ratings,
                           by = "id_produto")

analise_pagamento <- left_join(analise_pagamento, tb_customer_payments,
                               by = "id_cliente")

###########################################################

analise_vendas <- left_join(tb_addresses, tb_vendors,
                            by = c("id_fornecedor", "id_endereco"))

analise_vendas <- left_join(analise_vendas, tb_vendor_payments,
                            by = "id_fornecedor") 

analise_vendas <- left_join(analise_vendas, tb_products,
                            by = "id_fornecedor") 

analise_vendas <- left_join(analise_vendas, tb_ordered_products,
                            by = "id_produto") 

##salvar arquivos
save(analise_endereco, file = "analise_endereco.RData")
save(analise_pagamento, file = "analise_pagamento.RData")
save(analise_produtos, file = "analise_produtos.RData")
save(analise_usuario, file = "analise_usuario.RData")
############################################################


##Grafico em linhas
ggplot(data = analise_pedidos) +
  geom_bar(aes(x = metodo_pagamento), color = "black", fill = "aquamarine3") +
  geom_text(aes(x = metodo_pagamento, label = ..count..), 
            stat = "count", vjust = -1.05) +
  labs(title = "Métodos de Pagamentos Usados",
       x = "Método",
       y = "Quantidade") +
  theme_light()
 
#######################################################################

ggplot(data = analise_pagamento) +
  geom_bar(aes(x = status_pagamento), color = "black", fill = "aquamarine3") +
  geom_text(aes(x = status_pagamento, label = ..count..), 
            stat = "count", vjust = -1.05) +
  labs(title = "Status de Pagamentos dos Clientes",
       subtitle = "Banco X",
       x = "Pagamentos efetuados",
       y = "Quantidade") +
  theme_light()

#######################################################################

ggplot(analise_usuario) +
  geom_point(aes(x = delivery_raio_km, y = qtde_total_compra, 
                 size = qtde_total_compra)) +
  geom_smooth(aes(x = delivery_raio_km, y = qtde_total_compra), 
              method = "loess", se = FALSE) +
  labs(title = "Quantidade de Compra x Raio de Delivery",
       x = "Raio x Km",
       y = "Quantidade total") +
  theme_bw()

###############################################

ggplot(analise_usuario) +
  geom_point(aes(x = delivery_raio_km, y = ultima_qtde_compra, 
                 size = ultima_qtde_compra)) +
  geom_smooth(aes(x = delivery_raio_km, y = ultima_qtde_compra), 
              method = "loess", se = FALSE) +
  labs(title = "última quantidade comprada x Raio de km",
       x = "Raio de km",
       y = "Quantidade total") +
  theme_bw()

#############################################

ggplot(data = analise_pagamento) +
  geom_bar(aes(x = status_pagamento), color = "black", fill = "aquamarine3") +
  geom_text(aes(x = status_pagamento, label = ..count..), 
            stat = "count", vjust = -1.05) +
  labs(title = "Status de Pagamentos dos Clientes",
       subtitle = "Banco X",
       x = "Pagamentos efetuados",
       y = "Quantidade") +
  theme_light()





##############################################


#Analise da média de avaliação x valor de pagamento
#grafico de linha
ggplotly(ggplot(analise_pagamento) +
  geom_line(aes(x = avaliacao_media, y = valor, color = status_pagamento)) +
  geom_point(aes(x = avaliacao_media, y = valor, color = status_pagamento)) +
  labs(x = "Média de Avaliação",
       y = " Valor pago",
       color = "Status de Pagamento") +
  theme_bw())

########################################################

##análise avaliacao media x qtde compra

ggplot(analise_pagamento) +
           geom_line(aes(x = avaliacao_media, y = qtde_total_compra, color = status_pagamento)) +
           geom_point(aes(x = avaliacao_media, y = qtde_total_compra, color = status_pagamento)) +
           labs(x = "Avaliação de Média",
                y = " Quantidade Total de Compra",
                color = "Status Total de Compra") +
           theme_bw()


##################################

##análise do delivery x km com o gasto total do pedido

ggplot(analise_usuario) +
           geom_line(aes(x = delivery_raio_km, y = gasto_total, color = uf)) +
           geom_point(aes(x = delivery_raio_km, y = gasto_total, color = uf)) +
           labs(x = "Delivery x km",
                y = " Gasto Total",
                color = "UF") +
           theme_bw()

##################################

