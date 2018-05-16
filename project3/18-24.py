import numpy as np
from cvxopt import solvers, matrix

import matplotlib.pyplot as plt
import util
import pickle
import os


cwd = os.getcwd()

with open (cwd + '\\lambda.pk', 'rb') as f:
    lamb = pickle.load(f)
with open (cwd + '\\acc_1.pk', 'rb') as f:
    acc = pickle.load(f)

print(acc)
idx = np.argmax(acc)
best_lamb = lamb[idx]
print((best_lamb, acc[idx]))
util.plot_curve(lamb, acc, 'lambda v.s. acc for reward function2')
#===========================Question 20====================================
r_1, r_2 = util.initial_reward_map()
rfunc = r_2
mdp_process = util.return_new_mdp(rfunc)
util.plot_heat_map(rfunc, "Q-20-HeatMap for r2")
M = util.construct_matrix_constant(mdp_process)
c, D, b = util.get_c_D_b(best_lamb, M, rfunc)
calculated_reward = util.get_reward(c, D, b)
new_reward_map = util.create_reward_map(calculated_reward)
new_mdp = util.return_new_mdp(new_reward_map)
print (new_reward_map)
util.plot_heat_map(new_reward_map, "Q-20-HeatMap for best lambda r2")
#===========================Question 21=====================================
new_mdp.plot_value("Q-21-ValueMap for best lambda r2", text_reveal=True, decimal = 2)
#===========================Question 22=====================================
#Compare the heat maps of Question 3 and Question 14 and provide a brief explanation on their similarities and differences.
#===========================Question 23=====================================
new_mdp.plot_action("Q-23-Action Map for best lambda r2")
#===========================Question 24=====================================
#Compare
