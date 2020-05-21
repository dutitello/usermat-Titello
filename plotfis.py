import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from math import pi
from matplotlib.animation import FuncAnimation


class plotfis(object):
    def __init__(self, h, l, lant=0, nh=1, nl=1, nkptv=2, nkpth=2):
        self.h = h
        self.l = l
        self.lant = lant
        self.nh = nh
        self.nl = nl
        self.nkpth = nkpth
        self.nkptv = nkptv
        self.escfig = 1/15

    def escala(self, escala):
        self.escfig = escala


    def _plot_init_(self):
        # Determina propriedades

        # Dimensiona figura
        #figure(figsize=(self.l*self.escfig, self.h*self.escfig)).
        self.fig, self.ax = plt.subplots(figsize=(self.l*self.escfig, self.h*self.escfig))
        self.graf = self.ax.plot([],[])

        self.ax.set_xlim(-2*self.lant, 1.05*self.l)
        self.ax.set_ylim(-0.05*self.h, 1.15*self.h)

        # Abre arquivo CSV
        self.dados = pd.read_csv('_relpts.csv', sep=',', skiprows=1)
        self.dados = self.dados.drop_duplicates(subset=('elemId', 'Time', 'kDomIntPt', 'estado'), keep='last')

        # Determina tempo máximo
        #self.nlin = len(self.dados['Time'].values)
        self.tempomax = max(self.dados['Time'].values)

        #print('Némero de pontos no tempo: %d.' % (len(self.dados['Time'].values)/self.nlin))
        print('Tempo máximo (solução ANSYS): %f.' % (self.tempomax))

        # Retorna
        return self.graf



    def _plot_dados_(self, tempo):

        # Seleciona tempo
        if(tempo == 'max'):
            tempo = self.tempomax

        # Seleciona dados no tempo atual
        dadosti = self.dados.loc[self.dados['Time'] == tempo]

        # Cria contorno da viga
        self.graf = self.ax.fill([-self.lant, -self.lant, self.l, self.l, -self.lant], [0, self.h, self.h, 0, 0], linewidth=1, facecolor='lightgray', edgecolor='black', zorder=-1)

        # Filtra pontos por estado
        ptint  = dadosti.loc[dadosti['estado'] ==   0, ['coordsX', 'coordsY']]
        ptesm  = dadosti.loc[dadosti['estado'] == -10, ['coordsX', 'coordsY']]
        ptfis1 = dadosti.loc[dadosti['estado'] ==  11, ['coordsX', 'coordsY', 'theta']]
        ptfis2 = dadosti.loc[dadosti['estado'] ==  12, ['coordsX', 'coordsY', 'theta']]

        # Plota cada grupo de pontos - 15*escala pq escala original é 1/15
        tamanfis = 250*self.escfig*15
        tamanesm = 80*self.escfig*15
        tamanint = 5*self.escfig*15

        self.graf.append(self.ax.scatter(ptint['coordsX'].values, ptint['coordsY'].values, marker='.', c='black', s=tamanint))
        self.graf.append(self.ax.scatter(ptesm['coordsX'].values, ptesm['coordsY'].values, marker='d', c='black', s=tamanesm))

        # Fissuras com angulo, um por um
        for eachfis in ptfis1.values:
            self.graf.append(self.ax.scatter(eachfis[0], eachfis[1], marker=(2, 2, eachfis[2]*180/pi), c='black', s=tamanfis))

        for eachfis in ptfis2.values:
            self.graf.append(self.ax.scatter(eachfis[0], eachfis[1], marker=(4, 2, eachfis[2]*180/pi), c='black', s=tamanfis))

        temptext = 'Tempo = %8f/%8f.' % (tempo, self.tempomax)
        self.graf.append(self.ax.text(0.0, self.h*1.05, temptext, fontsize=14))

        return self.graf


    def plt_tempo(self, tempo='max'):
        self._plot_init_()
        self._plot_dados_(tempo)
        plt.show()


    def plt_anim(self, dt=75, repeat=False):
        self._plot_init_()
        tempos = self.dados['Time'].drop_duplicates().values
        anim = FuncAnimation(self.fig, self._plot_dados_, frames=tempos, blit=True, interval=dt, repeat=repeat, cache_frame_data=False)
        plt.show()



#import plotfis; pl=plotfis.plotfis(56,366/2,5); pl.plt_anim()


"""
exec(open('plotfis.py').read())
fig = plotafis(56, 366/2, 4, 5)
fig.plt_tempo(1.159583)
fig.plt_anim(dt=50)

"""





"""

# Propriedades da viga
h = 56
l = 366/2
nh = 4
nl = 5

# Numero de pontos de integração em X e Y
nkpth = 2
nkptv = 2

# Tempo a ser exibido
tempoi = 0.781667

# Escala da figura - transforma l e h em pixels?
escfig = 1/15

# Determina propriedades
dh = h/nh
dl = l/nl
nele = nh*nl
nkpt = nkpth*nkptv
nlin = nkpt*nele

# Dimensiona figura
plt.figure(figsize=(l*escfig, h*escfig))

# Cria contorno da viga
plt.fill([0, 0, l, l, 0], [0, h, h, 0, 0], linewidth=1, facecolor='lightgray', edgecolor='black', zorder=-1)

# Abre arquivo CSV
dados = pd.read_csv('_relpts.csv', sep=',', skiprows=1)

# Seleciona tempo 
if(tempoi == 'max'):
    tempoi = max(dados['Time'].values)

dadosti = dados.loc[dados['Time'] == tempoi]

# Seleciona ultimas nlinhas
ult = dados.iloc[-nlin:]
ult.reindex()

# Filtra pontos por estado
ptint =  ult.loc[ult['estado'] ==   0, ['coordsX', 'coordsY']]
ptesm =  ult.loc[ult['estado'] == -10, ['coordsX', 'coordsY']]
ptfis1 = ult.loc[ult['estado'] ==  11, ['coordsX', 'coordsY', 'theta']]
ptfis2 = ult.loc[ult['estado'] ==  12, ['coordsX', 'coordsY', 'theta']]

# Plota cada grupo de pontos
tamanfis = 250
tamanesm = 80
tamanint = 5

plt.scatter(ptint['coordsX'].values, ptint['coordsY'].values, marker='.', c='black', s=tamanint)
plt.scatter(ptesm['coordsX'].values, ptesm['coordsY'].values, marker='d', c='black', s=tamanesm)

# Fissuras com angulo, um por um
for eachfis in ptfis1.values:
    plt.scatter(eachfis[0], eachfis[1], marker=(2, 2, eachfis[2]*180/pi), c='black', s=tamanfis)

for eachfis in ptfis2.values:
    plt.scatter(eachfis[0], eachfis[1], marker=(4, 2, eachfis[2]*180/pi), c='black', s=tamanfis)


# Exibe
plt.show()

"""


"""



    def _plot_init_1(self):
        # Determina propriedades
        dh = self.h/self.nh
        dl = self.l/self.nl
        nele = self.nh*self.nl
        nkpt = self.nkpth*self.nkptv
        self.nlin = nkpt*nele

        # Dimensiona figura
        #figure(figsize=(self.l*self.escfig, self.h*self.escfig)).
        self.fig = plt.figure(figsize=(self.l*self.escfig, self.h*self.escfig)).subplots()

        #self.fig.set_xlim(0, 2*np.pi)
        #self.fig.set_ylim(-1, 1)

        # Abre arquivo CSV
        self.dados = pd.read_csv('_relpts.csv', sep=',', skiprows=1)

        # Retorna
        return self.fig

    def _plot_dados_1(self, tempo):

        # Seleciona tempo
        if(tempo == 'max'):
            tempo = max(self.dados['Time'].values)

        # Seleciona dados no tempo atual
        dadosti = self.dados.loc[self.dados['Time'] == tempo]

        # Cria contorno da viga
        self.fig.fill([0, 0, self.l, self.l, 0], [0, self.h, self.h, 0, 0], linewidth=1, facecolor='lightgray', edgecolor='black', zorder=-1)

        # Seleciona ultimas nlinhas - ultima iteração
        #   Na verdade é indiferente pq isso é tirado do solver antes da iteração acontecer, com dados da anteiror
        ult = dadosti.iloc[-self.nlin:]
        ult.reindex()

        # Filtra pontos por estado
        ptint =  ult.loc[ult['estado'] ==   0, ['coordsX', 'coordsY']]
        ptesm =  ult.loc[ult['estado'] == -10, ['coordsX', 'coordsY']]
        ptfis1 = ult.loc[ult['estado'] ==  11, ['coordsX', 'coordsY', 'theta']]
        ptfis2 = ult.loc[ult['estado'] ==  12, ['coordsX', 'coordsY', 'theta']]

        # Plota cada grupo de pontos - 15*escala pq escala original é 1/15
        tamanfis = 250*self.escfig*15
        tamanesm = 80*self.escfig*15
        tamanint = 5*self.escfig*15

        self.fig.scatter(ptint['coordsX'].values, ptint['coordsY'].values, marker='.', c='black', s=tamanint)
        self.fig.scatter(ptesm['coordsX'].values, ptesm['coordsY'].values, marker='d', c='black', s=tamanesm)

        # Fissuras com angulo, um por um
        for eachfis in ptfis1.values:
            self.fig.scatter(eachfis[0], eachfis[1], marker=(2, 2, eachfis[2]*180/pi), c='black', s=tamanfis)

        for eachfis in ptfis2.values:
            self.fig.scatter(eachfis[0], eachfis[1], marker=(4, 2, eachfis[2]*180/pi), c='black', s=tamanfis)

        return self.fig

"""


"""
EXEMPLO DE ANIMAÇÃO:

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

fig, ax = plt.subplots()
xdata, ydata = [], []
ln = plt.plot([], [], 'ro')

def init():
 ax.set_xlim(0, 2*np.pi)
 ax.set_ylim(-1, 1)
 return ln

def update(frame):
 xdata.append(frame)
 ydata.append(np.sin(frame))
 ln = plt.plot(xdata, ydata, 'ro')
 return ln

ani = FuncAnimation(fig, update, frames=np.linspace(0, 2*np.pi, 128), init_func=init, blit=True)
plt.show()

"""
