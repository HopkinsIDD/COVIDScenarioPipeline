import datetime
import time
import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from COVIDScenarioPipeline.SEIR import seir, setup, results


class SomeStateSpatialSetup():
    """
        Setup for Maryland at the county scale.
    """
    def __init__(self):
        folder = 'somestate/'
        self.data = pd.read_csv(f'data/{folder}geodata.csv')
        self.mobility = np.loadtxt(f'data/{folder}mobility.txt')
        self.popnodes = self.data['pop2010'].to_numpy()
        self.nnodes = len(self.data)
        #self.counties_shp = gpd.read_file(f'data/{folder}somestate-counties-shp/somestate.shp')
        #self.counties_shp.sort_values('GEOID', inplace=True)


if __name__ == '__main__':  # For windows thread

    s = setup.Setup(setup_name='mid-SomeState',
                    spatial_setup=SomeStateSpatialSetup(),
                    nsim=int(sys.argv[1]),
                    ti=datetime.date(2020, 3, 1),
                    tf=datetime.date(2020, 7, 1),
                    interactive=False,
                    write_csv=True)

    p = setup.COVID19Parameters(s.setup_name, s.nbetas)

    seeding_place = SomeGEOID
    seeding_amount = [3]
    s.buildIC(seeding_places=[
        int(s.spatset.data[s.spatset.data['geoid'] == seeding_place].id)
    ],
              seeding_amount=seeding_amount)

    #s.set_filter(np.loadtxt('data/california/filter_github.txt')/100)

    tic = time.time()
    seir.onerun_SEIR(s, p, 0)
    print(f">>> Compilation done in {time.time()-tic} seconds...")

    seir = seir.run_parallel(s, p)

    results = results.Results(s, seir)

    simR = results.save_output_for_R(seir)

    results.plot_quick_summary()

    results.build_comp_data()  # Long !!

    nodes_to_plot = [
        int(s.spatset.data[s.spatset.data['geoid'] == SomeGEOID].id),
        int(s.spatset.data[s.spatset.data['geoid'] == SomeGEOID1].id)
    ]

    fig, axes = results.plot_all_comp(nodes_to_plot)
    fig.autofmt_xdate()

    results.plot_comp_mult('cumI', nodes_to_plot)
    fig, axes = results.plot_comp('cumI', nodes_to_plot)

    if s.interactive:
        plt.show()
