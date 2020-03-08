import numpy as np
import pandas as pd
import datetime, time, multiprocessing, itertools
import matplotlib.pyplot as plt
import seir, setup, results




s = setup.Setup(setup_name = 'midSD',
                #spatial_setup = setup.CaliforniaSpatialSetup(),
                spatial_setup = setup.WestCoastSpatialSetup(),
                nsim = 10, 
                ti = datetime.date(2020, 1, 23),
                tf = datetime.date(2020, 4, 1),
                interactive = False,
                write_csv = False)

p = setup.COVID19Parameters(s.setup_name, s.nbetas)

seeding_place = 6075  # SF_GEOID:6075   # SC_GEOID:6067
seeding_amount = [3]
s.buildIC(seeding_places = [int(s.spatset.data[s.spatset.data['geoid'] == seeding_place].id)], 
          seeding_amount = seeding_amount)

#s.set_filter(np.loadtxt('data/california/filter_github.txt')/100)

tic = time.time()
seir.onerun_SEIR(s, p, 0)
print(f">>> Compilation done in {time.time()-tic} seconds...")

seir = seir.run_parallel(s, p)

results = results.Results(s, seir)

simR = results.save_output_for_R(seir)
            
results.build_comp_data()

nodes_to_plot = [int(s.spatset.data[s.spatset.data['geoid']== 6067].id), 
                 int(s.spatset.data[s.spatset.data['geoid'] == 6075].id)]

fig, axes = results.plot_all_comp(nodes_to_plot)
fig.autofmt_xdate()

results.plot_comp_mult('cumI', nodes_to_plot)
fig, axes = results.plot_comp('cumI', nodes_to_plot)

results.plot_quick_summary()

if s.interactive:
    plt.show()