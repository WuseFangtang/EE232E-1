import numpy as np
from cvxopt import solvers, matrix

import matplotlib.pyplot as plt
import util
import pickle
import os

#============================Question 12=================================
cwd = os.getcwd()

with open (cwd + '\\lambda.pk', 'rb') as f:
    lamb = pickle.load(f)
with open (cwd + '\\acc_0.pk', 'rb') as f:
    acc = pickle.load(f)

print(acc)
idx = np.argmax(acc)
best_lamb = lamb[idx]
print((best_lamb, acc[idx]))
util.plot_curve(lamb, acc, 'lambda v.s. acc for reward function1')
#===========================Question 13====================================
r_1, r_2 = util.initial_reward_map()
rfunc = r_1
mdp_process = util.return_new_mdp(rfunc)
util.plot_heat_map(r_1, "Q-13-HeatMap for r1")
M = util.construct_matrix_constant(mdp_process)
c, D, b = util.get_c_D_b(best_lamb, M, rfunc)
calculated_reward = util.get_reward(c, D, b)
new_reward_map = util.create_reward_map(calculated_reward)
new_mdp = util.return_new_mdp(new_reward_map)
print (new_reward_map)
util.plot_heat_map(new_reward_map, "Q-13-HeatMap for best lambda r1")
#===========================Question 14=====================================
new_mdp.plot_value("Q-14-ValueMap for best lambda r1", text_reveal=True, decimal = 2)
#===========================Question 15=====================================
#Compare the heat maps of Question 3 and Question 14 and provide a brief explanation on their similarities and differences.
#===========================Question 16=====================================
new_mdp.plot_action("Q-16-Action Map for best lambda r1")
#===========================Question 17=====================================
#Compare
