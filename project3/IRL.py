import numpy as np
from cvxopt import solvers, matrix

import matplotlib.pyplot as plt
import util
import pickle
import os


def acc_calc(expert_action, new_action, lambda_range):
    acc = []
    for i in range(len(lambda_range)):
        accuracy = 0
        for j in range(10):
            for k in range(10):
                if expert_action[j][k] == new_action[i][j][k]:
                    accuracy += 1
        accuracy /= 100
        acc.append(accuracy)
        print('lambda = ' + str(lambda_range[i]) + ':  ' + str(accuracy))
    return acc

lambda_range = np.linspace(0, 5, 500)
r_1, r_2 = util.initial_reward_map()

action_dict = {0:'right', 1:'up', 2:'left', 3:'down'}
action_list = ['right','up','left','down']

for r_idx, rfunc in enumerate([r_1, r_2]):
    mdp_process = util.return_new_mdp(rfunc)
    action_data1 = mdp_process.action
    print (action_data1)

    process = []
    new_action = []
    M = util.construct_matrix_constant(mdp_process)

    for idx, coeffi_lambda in enumerate(lambda_range):
        print('Now is doing: ' + str(idx))
        c,D,b = util.get_c_D_b(coeffi_lambda, M, rfunc)
        calculated_reward = util.get_reward(c,D,b)
        new_reward_map = util.create_reward_map(calculated_reward)
        new_mdp = util.return_new_mdp(new_reward_map)
        process.append(new_mdp)
        new_action.append(new_mdp.action)
        #print (new_mdp.action)

    acc = acc_calc(action_data1, new_action, lambda_range)
    cwd = os.getcwd()
    with open(cwd + '\\lambda.pk', 'wb') as f:
        pickle.dump(lambda_range, f)
    with open (cwd + '\\acc_' + str(r_idx)+'.pk', 'wb') as f:
        pickle.dump(acc,f)






















