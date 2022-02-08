import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


class Results():
    def __init__(self, s, seir):
        self.s = s
        self.seir = seir
        self.ti = self.s.ti
        self.tf = self.s.tf

        self.freq = str(s.dt * 24) + 'H'
        #self.timestamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
        self.figdir = f'model_output/{self.s.setup_name}/figures/'
        if not os.path.exists(self.figdir):
            os.makedirs(self.figdir)

    def plot_quick_summary(self, comp='cumI'):
        expand_data = [0] * self.s.nnodes

        for nd in range(self.s.nnodes):
            expand_data[nd] = {}
            expand_data[nd][comp] = pd.DataFrame(
                index=pd.date_range(self.ti, self.tf, freq=self.freq),
                columns=np.arange(self.s.nsim))
            for sim in range(self.s.nsim):
                expand_data[nd][comp][sim] = self.seir[sim][cumI][nd]

        fig, ax = plt.subplots(1, 1, figsize=(10, 5), sharey=True, sharex=True)

        for i, nd in enumerate(np.arange(self.s.nnodes)):
            ax.plot(expand_data[nd][comp].quantile(.5, axis=1))
            ax.fill_between(expand_data[nd][comp].index,
                            expand_data[nd][comp].quantile(.05, axis=1),
                            expand_data[nd][comp].quantile(.95, axis=1),
                            alpha=.3)
        fig.autofmt_xdate()
        if not self.s.interactive:
            plt.savefig(
                f'{self.figdir}{self.s.setup_name}_{comp}_per_node{self.timestamp}.pdf'
            )

        q50 = pd.DataFrame(index=pd.date_range(self.ti,
                                               self.tf,
                                               freq=self.freq),
                           columns=np.arange(self.s.nnodes))
        q05 = pd.DataFrame(index=pd.date_range(self.ti,
                                               self.tf,
                                               freq=self.freq),
                           columns=np.arange(self.s.nnodes))
        q95 = pd.DataFrame(index=pd.date_range(self.ti,
                                               self.tf,
                                               freq=self.freq),
                           columns=np.arange(self.s.nnodes))
        for i, nd in enumerate(np.arange(self.s.nnodes)):
            q50[i] = expand_data[nd][comp].quantile(.5, axis=1)
            q95[i] = expand_data[nd][comp].quantile(.95, axis=1)
            q05[i] = expand_data[nd][comp].quantile(.05, axis=1)

        fig, ax = plt.subplots(1, 1, figsize=(7, 7), sharey=True, sharex=True)

        ax.plot(q50.sum(axis=1))
        ax.fill_between(q50.index, q05.sum(axis=1), q95.sum(axis=1), alpha=.3)
        fig.autofmt_xdate()
        if not self.s.interactive:
            plt.savefig(
                f'{self.figdir}{self.s.setup_name}_{comp}_all_nodes{self.timestamp}.pdf'
            )

    def build_comp_data(self):
        """ Very long"""

        # Build Quantiles:
        expand_data = [0] * self.s.nnodes

        nsim = self.s.nsim
        for nd in range(self.s.nnodes):
            expand_data[nd] = {}
            expand_data[nd]['S'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                columns=np.arange(nsim))
            expand_data[nd]['E'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                columns=np.arange(nsim))
            expand_data[nd]['I1'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                 columns=np.arange(nsim))
            expand_data[nd]['I2'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                 columns=np.arange(nsim))
            expand_data[nd]['I3'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                 columns=np.arange(nsim))
            expand_data[nd]['R'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                columns=np.arange(nsim))
            expand_data[nd]['cumI'] = pd.DataFrame(index=pd.date_range(
                self.ti, self.tf, freq=self.freq),
                                                   columns=np.arange(nsim))
            for sim in range(self.s.nsim):
                expand_data[nd]['S'][sim] = self.seir[sim][S][nd]
                expand_data[nd]['E'][sim] = self.seir[sim][E][nd]
                expand_data[nd]['I1'][sim] = self.seir[sim][I1][nd]
                expand_data[nd]['I2'][sim] = self.seir[sim][I2][nd]
                expand_data[nd]['I3'][sim] = self.seir[sim][I2][nd]
                expand_data[nd]['R'][sim] = self.seir[sim][R][nd]
                expand_data[nd]['cumI'][sim] = self.seir[sim][cumI][nd]

        self.comp_data = expand_data
        self.colors = ['r', 'b', 'y', 'k', 'orange']
        self.figsize = (5, 5)

    def save_output_for_R(self, seir):
        """
        This is very ugly...
        """
        #self.datadir = f'model_output/{self.s.setup_name}/{self.s.setup_name}_{self.timestamp}/'
        #if not os.path.exists(self.datadir):
        #    os.makedirs(self.datadir)

        sims = []
        for s in range(self.s.nsim):
            a = seir[s].copy()[:, :, ::int(1 / self.s.dt)]
            a = np.moveaxis(a, 1, 2)
            a = np.moveaxis(a, 0, 1)
            b = np.diff(a, axis=0)
            difI = np.zeros((self.s.n_days, self.s.nnodes))
            difI[1:, :] = b[:, cumI, :]
            na = np.zeros((self.s.n_days, ncomp + 1, self.s.nnodes))
            na[:, :-1, :] = a
            na[:, -1, :] = difI

            m, n, r = na.shape
            out_arr = np.column_stack((np.tile(np.arange(n),
                                               m), na.reshape(n * m, -1)))

            out_df = pd.DataFrame(
                out_arr,
                columns=['comp'] +
                list(self.s.spatset.data['geoid'].astype(int)),
                index=pd.date_range(self.ti, self.tf,
                                    freq='D').repeat(ncomp + 1))
            out_df['comp'].replace(S, 'S', inplace=True)
            out_df['comp'].replace(E, 'E', inplace=True)
            out_df['comp'].replace(I1, 'I1', inplace=True)
            out_df['comp'].replace(I2, 'I2', inplace=True)
            out_df['comp'].replace(I3, 'I3', inplace=True)
            out_df['comp'].replace(R, 'R', inplace=True)
            out_df['comp'].replace(cumI, 'cumI', inplace=True)
            out_df['comp'].replace(ncomp, 'diffI', inplace=True)

            if self.s.write_csv:
                out_df.to_csv(
                    f"{self.datadir}{self.s.setup_name}_sim_{s}_scn.csv",
                    index='time',
                    index_label='time')
            sims.append(out_df)

        return sims

    def plot_comp(self, comp, nodes):

        fig, axes = plt.subplots(1, 1, figsize=self.figsize)

        for i, nd in enumerate(nodes):
            axes.plot(self.comp_data[nd][comp].quantile(.5, axis=1),
                      c=self.colors[i % 4],
                      label=self.s.spatset.data['geoid'][nd])
            axes.fill_between(self.comp_data[nd][comp].index,
                              self.comp_data[nd][comp].quantile(.05, axis=1),
                              self.comp_data[nd][comp].quantile(.95, axis=1),
                              alpha=.3,
                              facecolor=self.colors[i % 4])

        axes.legend()
        fig.autofmt_xdate()

        if not self.s.interactive:
            plt.savefig(
                f'{self.figdir}{self.s.setup_name}_{comp}_selected{self.timestamp}.pdf'
            )

        return fig, axes

    def plot_all_comp(self, nodes):
        fig, axes = plt.subplots(2, 4, figsize=(15, 7))

        for c, comp in enumerate(['S', 'E', 'I1', 'I2', 'I3', 'R', 'cumI']):
            ax = axes.flat[c]
            ax.set_title(comp)
            for i, nd in enumerate(nodes):
                ax.plot(self.comp_data[nd][comp].quantile(.5, axis=1),
                        c=self.colors[i % 4],
                        label=self.s.spatset.data['geoid'][nd])
                ax.fill_between(self.comp_data[nd][comp].index,
                                self.comp_data[nd][comp].quantile(.05, axis=1),
                                self.comp_data[nd][comp].quantile(.95, axis=1),
                                alpha=.3,
                                facecolor=self.colors[i % 4])

        ax.legend()
        fig.autofmt_xdate()
        if not self.s.interactive:
            plt.savefig(
                f'{self.figdir}{self.s.setup_name}_allcomp_selected{self.timestamp}.pdf'
            )
        return fig, axes

    def plot_comp_mult(self, comp, nodes):
        fig, axes = plt.subplots(len(nodes) // 2,
                                 len(nodes) // 2 + 1,
                                 figsize=(10, 5),
                                 sharey=True,
                                 sharex=True)

        for i, nd in enumerate(nodes):
            ax = axes.flat[i]
            ax.set_title(self.s.spatset.data['geoid'][nd])
            ax.plot(self.comp_data[nd][comp].quantile(.5, axis=1),
                    c=self.colors[i % 4])
            ax.fill_between(self.comp_data[nd][comp].index,
                            self.comp_data[nd][comp].quantile(.05, axis=1),
                            self.comp_data[nd][comp].quantile(.95, axis=1),
                            alpha=.3,
                            facecolor=self.colors[i % 4])
            fig.autofmt_xdate()

        if not self.s.interactive:
            plt.savefig(
                f'{self.figdir}{self.s.setup_name}_{comp}2_selected{self.timestamp}.pdf'
            )
        return fig, axes
