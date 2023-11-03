# Tese-Portfolio
Este repositório contém o código para reproduzir os resultados da Tese de Doutorado Alocação ótima de renováveis: propostas de melhorias à Teoria Moderna do Portfólio aplicada a sistemas elétricos.

#### Resumo:

> Com a tendência do aumento da participação de fontes renováveis variáveis na matriz elétrica e considerando a flexibilidade relativamente reduzida dessas tecnologias, há a necessidade de se reduzir a variabilidade da geração agregada do sistema. Uma das formas de se fazer isso é através da combinação de usinas com perfis de geração complementares. Para se obter uma combinação ótima de usinas, uma metodologia aplicada é a Teoria Moderna do Portfólio (MPT). Entretanto, as adaptações dessa metodologia, encontradas na literatura, aplicadas ao setor elétrico não consideram alguns aspectos importantes. Para tratar esses problemas, são propostas alterações metodológicas para o uso da MPT aplicada a sistemas elétricos. Propõe-se incorporar os custos, considerar a demanda e tecnologias controláveis e usar uma métrica de risco que penalize apenas desvios negativos. Cada uma das alterações propostas é testada e seus impactos nos resultados são apresentados e discutidos. Conclui-se que as alterações propostas podem alterar significativamente a composição dos portfólios ótimos. Carteiras da fronteira eficiente da formulação tradicional com alto desvio padrão da geração não são capazes de fornecer um nível de energia firme a um custo competitivo. A consideração de tecnologias controláveis altera a composição dos portfólios e pode reduzir o custo do sistema. Além disso, é mostrado que a diversificação tem um papel importante para a redução da variabilidade das fontes variáveis, embora a presença de termelétricas possa reduzir a necessidade de diversificação.

## Requisitos

-   R
-   CPLEX

Se os pacotes necessários não estiverem instalados, execute `install.packages(c("tidyverse", "corrr", "lubridate", "lhs", "furrr", "geosphere", "ggrepel"))` no R.

## Arquivos de entrada

Os seguintes arquivos são necessários para rodar o modelo:

-   **parameters.R**: Define os principais parâmetros para a execução.
-   **PlantData_v2.rds**: Dados das usinas, exceto as séries temporais.
-   **WindSolar.rds**: Séries temporais para usinas eólicas e fotovoltaicas. Disponível em <https://zenodo.org/record/4924020>.
-   **SeriesComplete.rds**: Séries temporais para usinas eólicas, fotovoltaicas e as criadas para termelétricas com patamares mínimos de 10%, 30% e 60% e armazenamento com 1, 5 e 25 horas de capacidade. Disponível em https://mega.nz/file/Ix9zTLoK#l-sxVq5_dAVMBjX03SNOw6p1V81Ibd0gsMu4u61cMm0. A criação das séries temporais a partir das séries de usinas renováveis (arquivo **WindSolar.rds**) pode ser reproduzida conforme descrito abaixo.

## Criação das séries temporais de tecnologias controláveis.

O arquivo **SeriesComplete.rds** já contém as séries temporais para todas as usinas renováveis e tecnologias controláveis. Mas, se desejar reproduzir o resultado ou criar séries com outros parâmetros, siga as instruções abaixo.
Para rodar esta parte, é necessário o pacote cplexAPI. Infelizmente, o pacote deixou de estar disponível no CRAN recentemente, mas ainda funciona a versão 1.4.0 arquivada em https://cran.r-project.org/src/contrib/Archive/cplexAPI/.
Abra o arquivo **src/CriaSériesArmazTerm.R** e digite Ctrl+Shift+S (Source).

## Como rodar otimização e figuras da Tese

### A partir da linha de comando

    Rscript main.R

### A partir do Rstudio

Abra o arquivo **main.R** e digite Ctrl+Shift+S (Source).

### Localização do CPLEX

O caminho para o executável do CPLEX deve ser informado no arquivo **parameters.R** antes de rodar o script. No Linux, a pasta padrão da instalação (na versão 12.10) é **/opt/ibm/ILOG/CPLEX_Studio1210/opl/bin/x86-64_linux/**. Se o CPLEX não estiver disponível, é possível baixar o arquivo **Results.rds** contendo os resultados. Com isso, é possível analisar os resultados e gerar as figuras da Tese. This file contains the results from an execution using **CPLEX** 12.10.0, **R** 4.0.3, **tidyverse** 1.3.0, **lubridate** 1.7.9.2, **lhs** 1.1.1, **furrr** 0.2.2, **corrr** 0.4.3, **geosphere** 1.5-10. Para carregar os dados e gerar as figuras sem o CPLEX, há duas possibilidades:

-   Rodar `Rscript main.R noCPLEX`, na linha de comando.
-   Alterar a variável *CPLEX* para FALSE no arquivo **parameters.R**.

### Número de núcleos

Altere a variável **NumThreads** no arquivo **parameters.R** para definir o número de processadores a serem usados em algumas rotinas. Essa opção não tem impacto na otimização do CPLEX. Cada processador requer memória RAM, então, não se recomenda usar valores muito altos, a não ser que o equipamento tenha bastante memória RAM disponível.

## Saída esperada

Depois de rodar o script, o arquivo **Resultados.rds** é criado na pasta **results/**. O nome e a localização do arquivo **Resultados.rds** podem ser alterados no arquivo **parameters.R** antes da execução do script. Todas as figuras são exportadas para o formato PDF.

### Diferença nos resultados

Os resultados da otimização podem variar levemente, dependendo das configurações do sistema, sistema operacional, versão do CPLEX, etc.No entanto, essas diferenças são mínimas.

## Estrutura dos resultados

O arquivo **Resultadoss.rds** contém uma lista que contém as tabelas (tibbles) descritas abaixo.

-   **ExecParam**: Cada linha é um cenário e os parâmetros usados para rodá-lo.
-   **MainResults**: Cada linha é uma execução da otimização, ou seja, um ponto da fronteira eficiente para aquele cenário. Todos os resultados estão aqui, excetos os relacionados às usinas individualmente. Exemplo: custos, desvio padrão, geração média do portfólio, etc.
-   **PlantResults**: Capacidade instalada de cada usina para cada rodada.
-   **MultiplierResults**: VaR, CVaR e multiplicador que atenda ao critério de CVaR para cada execução.
-   **DivIndex**: Índices de diversidade calculados.

## Erros e dúvidas
Qualquer erro, dúvida ou problema, favor abrir um issue.
