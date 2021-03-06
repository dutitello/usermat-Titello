# usermat-Titello

Essa usermat está particionada em vários arquivos e subrotinas, para usar basta compilar o arquivo usermat que o compilador encontra o resto se estiver na mesma pasta. Não há nada que impeça de juntar todas as rotinas no mesmo arquivo, montei assim para facilitar a programação e organização.

Minha usermat é para estado plano (`usermatps`) e considera concreto com ou sem fibras de aço, explicações sobre a entrada de dados estão na planilha `vars_estado.xlsx`.
A organização é essencialmente: no arquivo `usermatps` esta o corpo da subrotina que tecnicamente não precisará sofrer grandes alterações com o desenvolvimento do código, quase tudo que precisa ser mudado é chamado em subrotinas que chamam outras subrotinas (isso facilita muito a manutenção do código). A edição do vetor de variáveis de estado é realizada pelas `"rotinas_estado"`, assim posso apenas carregar o vetor de `ustatev` para todos lados e trabalhar como se fosse um programa orientado a objetos, e ainda posso alterar facilmente a posição dos dados no vetor. Caso as propriedades do concreto variem no tempo/solução deve ser removido o trecho das rotinas de estado que calcula apenas na primeira vez os valores dos parametros (exemplo: a linha 342 do arquivo `rotinas_estado` onde `calc` testa se os parametros são conhecidos, se eles variam isso deve ser recalculado em cada etapa).

A construção do código foi baseada no trabalho da Prof. Paula M. Lazzari, mas foi tudo reescrito e comentado com minhas interpretações (talvez erradas...). 

O algoritmo de fissuração foi reduzido para o estado plano podendo ter apenas duas fissuras perpendiculares (cálcula a primeira e a segunda está a 90°). Foi ainda criado um critério no Tension-stiffening para permitir a falha por corte em vigas sem estribos (ver minha dissertação pra isso).

O algoritmo plástico foi muito alterado, foram criadas funções para determinar o numero de plasticidade `f` em função da superfície de ruptura e da regra de endurecimento (que são subrotinas), assim posso derivar `f` numéricamente e aplicar Newton-Raphoson + line search para encontrar a deformação de endurecimento (acredito que a deformação plástica equivalente tenha algum problema em sua formulação, mas isso foi ignorado e mantido igual aos demais trabalhos). O algoritmo plástico foi construído no estado plano, mas a maioria das funções depende apenas das tensões principais e invariantes de tensão, asssim considerando todas componentes nessas etapas o algoritmo deve funcionar em 3D. As propriedades do concreto são parametros determinados por rotinas adjacentes, então podem ser alteradas sem mexer no código principal, isso deve facilitar a consideração de visco e de problemas térmicos. 

Existem alguns exemplos na pasta `apdls`, mas estão incompletos.

.

Para fazer download de todos os arquivos [clique aqui](https://github.com/dutitello/usermat-Titello/archive/master.zip).

Recomendo ainda usar o debbuger do VS para trabalhar com isso, [tutorial aqui](https://github.com/dutitello/debug-ansys-upf/).

.

Boa sorte!

Eduardo P. Titello, 

maio de 2020

.

PS: O arquivo em Python plota a fissuração com o avanço da carga, mas pra isso precisa compilar o user01.f, ativar a exportação do estágio de fissuração e configurar o script em Python para ler, não é nada muito dificil, mas não é para principiantes...
