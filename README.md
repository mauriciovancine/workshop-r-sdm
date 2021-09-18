# Repositório da Oficina 3 - MOCÓ - Mastozóologos Organizados em uma Conferência Online

## MINICURSO 3: Introdução à modelagem de distribuição de espécies usando a linguagem R 

**Ministrante**: Maurício Humberto Vancine

**Instituição**: Universidade Estadual Paulista (UNESP), Câmpus de Rio Claro, Rio Claro, SP

**Resumo**

A ampla disponibilidade de dados sobre a biodiversidade e variáveis ambientais propiciam o uso de análises espaciais, dentre elas, os Modelos de Distribuição de Espécies (ou do inglês Species Distribution Modeling — SDM). Nessa oficina, oferecerei uma introdução teórica e prática à técnica de SDM utilizando a linguagem R. Primeiramente, apresentarei brevemente os principais conceitos da teoria de nicho ecológico (Grinnell, Elton e Hutchinson) e da teoria de SDMs (espaço geográfico (G), espaço ambiental (E) e diagrama Biótico-Abiótico-Movimentação (BAM)). Seguida à parte teórica, apresentarei as principais bases de dados (ocorrências e variáveis ambientais), tipos de algoritmos (BIOCLIM, Mahalanobis, Gower, GLM, GAM, Random Forest, SVM e MaxEnt), avaliação dos modelos (ROC, AUC e TSS), limites de corte (thresholds) e consenso de modelos (ensemble). A parte prática será focada na construção dos modelos através da linguagem R, onde abordarei: (1) introdução à linguagem R, (2) obtenção e seleção de dados de ocorrências e variáveis ambientais, (3) ajuste e predição dos modelos, (4) métricas de avaliação, (5) consenso dos modelos (ensemble) e (6) composição dos mapas.

---

### Informações aos participantes

**Datas**:
<br>
25/09/2019 (9:00 h - 12:00 h)
<br> 
26/09/2019 (9:00 h - 12:00 h)

**Ementa**: [Ementa](https://github.com/mauriciovancine/workshop-r-sdm/blob/master/00_plano_ensino/plano_ensino_workshop_r_sdm.pdf)

**Contato**: 
Para mais informações ou dúvidas, envie e-mail para:

- [Organização do MOCÓ](https://moco2021.com.br/home/comissao-organizadora/)
- Maurício Vancine (mauricio.vancine@gmail.com)

---

### Instruções aos participantes

**Hardware** <br>
Será necessário que todas e todos usem seus próprios notebooks.

**Softwares**<br>R e RStudio <br>
Instalar a versão mais recente do R (4.1.1): https://www.r-project.org/ <br>
Instalar o RStudio: https://www.rstudio.com/ <br>
Vídeo de instalação do R e do RStudio: https://youtu.be/l1bWvZMNMCM <br>

Para quem não tem familiaridade com a linguagem R, sugiro que se possível, assistam algumas aulas desse curso: http://www.bosontreinamentos.com.br/category/programacao-em-r/

**Instalar os pacotes no R** <br>
Com o R e o RStudio instalados, baixe esse [script](https://github.com/mauriciovancine/workshop-r-sdm/blob/master/02_scripts/script_r_instalar_pacotes.R) (scripts são roteiros que possuem comandos, como um rascunho). <br>
Abra o script baixado (**script_r_instalar_pacotes.R**) no software RStudio e rode cada linha de comando para instalar os pacotes. <br>
Para rodar as linhas, basta colocar o cursor na linha de código a ser executada e precionar: `Crtl + Enter`, como mostra o gif abaixo:

![Alt Text](https://appsilon.com/wp-content/uploads/2019/03/blog_code_execution_optimized.gif)

**Dúvidas ou dificuldades** <br>
Para ajuda, envie e-mail para: <br>

- Maurício Vancine (mauricio.vancine@gmail.com)
