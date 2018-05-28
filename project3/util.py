import numpy as np
from cvxopt import matrix, solvers
import matplotlib.pyplot as plt
import pickle
import os

import copy

import random
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter

def plot_reward_map(reward, file_name):
    fig = plt.figure()
    ax = plt.gca()
    ax.invert_yaxis()
    ax.set_xticks(np.arange(0, 11, 1))
    ax.set_yticks(np.arange(0, 11, 1))
    plt.pcolor(reward, cmap = "inferno")
    plt.colorbar()
    plt.grid()
    plt.savefig(file_name)

class MDP():
    def __init__(self, V=None, delta=None, epsilon=None, gamma=None,
                 w=None, reward=None, action=None):
        self.V = V  # V is reward matrix
        self.delta = None
        self.epsilon = epsilon
        self.gamma = gamma
        self.w = w
        self.reward = reward
        self.action = None
        self.transition_mat_dict = {} # this dictionary saves the transition prob matrix for a given action
                                      # For example self.transition_mat_dict['up'] stands for action = 'up'

        self.updated = 0
        self.m = 10
        self.n = 10

        self.transition_mat_dict['up'] = np.zeros([100, 100])
        self.transition_mat_dict['down'] = np.zeros([100, 100])
        self.transition_mat_dict['right'] = np.zeros([100, 100])
        self.transition_mat_dict['left'] = np.zeros([100, 100])

    def initialize_prob_trans_matrix(self):
        [m, n] = self.reward.shape
        for x in range(m):
            for y in range(n):
                state_number = m * y + x
                for actions in ['up','down','left','right']:
                    if (actions == "up"):
                        p1 = 1 - 0.75 * self.w  # up
                        p2 = 0.25 * self.w  # left
                        p3 = 0.25 * self.w  # right
                        p4 = 0.25 * self.w  # down
                    if (actions == "down"):
                        p1 = 0.25 * self.w
                        p2 = 0.25 * self.w
                        p3 = 0.25 * self.w
                        p4 = 1 - 0.75 * self.w
                    if (actions == "left"):
                        p1 = 0.25 * self.w
                        p2 = 1 - 0.75 * self.w
                        p3 = 0.25 * self.w
                        p4 = 0.25 * self.w
                    if (actions == "right"):
                        p1 = 0.25 * self.w
                        p2 = 0.25 * self.w
                        p3 = 1 - 0.75 * self.w
                        p4 = 0.25 * self.w
                    self.modify_prob_transition_matrix(x,y,actions,p1,p2,p3,p4)

    '''
        Functions: modify_prob_transition_matrix
        Pa(i ; j) = P(st+1 = j | st = i ; at = a)
    '''
    def modify_prob_transition_matrix(self,x, y, action, p1, p2, p3, p4): #up left right down
        [m, n] = self.reward.shape
        state_number = m * y + x
        up_state_number = m * y + max(x - 1, 0)
        down_state_number = m * y + min(x + 1, m - 1)
        left_state_number = m * (max(y - 1, 0)) + x
        right_state_number = m * (min(y + 1, n - 1)) + x

        self.transition_mat_dict[action][state_number][up_state_number] += p1
        self.transition_mat_dict[action][state_number][left_state_number] += p2
        self.transition_mat_dict[action][state_number][right_state_number] += p3
        self.transition_mat_dict[action][state_number][down_state_number] += p4


    def value_clac(self, x, y, actions):
        [m, n] = self.reward.shape
        state_number = m * y + x
        if (actions == "up"):
            p1 = 1 - 0.75 * self.w  # up
            p2 = 0.25 * self.w  # left
            p3 = 0.25 * self.w  # right
            p4 = 0.25 * self.w  # down

        if (actions == "down"):
            p1 = 0.25 * self.w
            p2 = 0.25 * self.w
            p3 = 0.25 * self.w
            p4 = 1 - 0.75 * self.w

        if (actions == "left"):
            p1 = 0.25 * self.w
            p2 = 1 - 0.75 * self.w
            p3 = 0.25 * self.w
            p4 = 0.25 * self.w

        if (actions == "right"):
            p1 = 0.25 * self.w
            p2 = 0.25 * self.w
            p3 = 1 - 0.75 * self.w
            p4 = 0.25 * self.w


        step_award = p1 * (self.reward[max(x - 1, 0), y] + self.gamma * self.V[max(x - 1, 0), y]) \
                     + p2 * (self.reward[x, max(y - 1, 0)] + self.gamma * self.V[x, max(y - 1, 0)]) \
                     + p3 * (self.reward[x, min(y + 1, n - 1)] + self.gamma * self.V[x, min(y + 1, n - 1)]) \
                     + p4 * (self.reward[min(x + 1, m - 1), y] + self.gamma * self.V[min(x + 1, m - 1), y])
        return (step_award)

    '''
    Function: look_forward(self,x,y) 
    Descriptin: Looking for a best action for a state with position x=x,y=y
    Args:
        x: x position of the state
        y: y position of the state
    Returns:
        max_temp: Reward value of the opitimal action
        ind: index of the action.(0:right, 1:up, 2:left, 3:down)
    '''

    def look_forward(self, x, y):
        ind_x = x
        # print(x)
        ind_y = y
        # print(y)
        v_up = self.value_clac(x=ind_x, y=ind_y, actions="up")
        v_dn = self.value_clac(x=ind_x, y=ind_y, actions="down")
        v_lft = self.value_clac(x=ind_x, y=ind_y, actions="left")
        v_rt = self.value_clac(x=ind_x, y=ind_y, actions="right")

        # tmp = [v_up, v_dn, v_lft, v_rt]
        # tmp = [v_rt, v_up, v_lft, v_dn]
        tmp = [v_lft, v_up, v_rt, v_dn]
        # print(tmp)
        max_tmp = max(tmp)
        ind_list = [i for i, j in enumerate(tmp) if j == max_tmp]
        if len(ind_list) > 1:
            #print("shoosh>>>{}>{}".format(x,y))
            ind = np.random.choice(ind_list, 1)[0]
            #print("choice:{}".format(ind))
        else:
            ind = tmp.index(max(tmp))
        return (max_tmp, ind)

    def update(self):
        if self.reward is None:
            raise Exception("Initialize reward function!")
        else:
            [m, n] = self.reward.shape
            self.V = np.zeros((10, 10))
            self.delta = 1e20  # shuld be a very large number
            self.action = np.zeros((10, 10))  # action matrix
           # print("Runnning Value iteration algorithm ... ")
            iterations = 0
            while self.delta > self.epsilon:
                self.delta = 0
                iterations = iterations + 1
                for i in np.arange(0, m):
                    for j in np.arange(0, n):
                        v = self.V[i, j]
                        update_value, opt_action = self.look_forward(x=i, y=j)
                        self.V[i, j] = update_value
                        self.action[i, j] = opt_action
                        self.delta = max(self.delta, np.abs(update_value - v))

                if (iterations % 10 == 0):
                    print(iterations)
                    print("delta = {}".format(self.delta))

          #  print("Optimum delta = {}".format(self.delta))
          #  print("Done!")
            self.initialize_prob_trans_matrix()

    def plot_action(self, filename):
        #
        [m, n] = self.reward.shape
        X = np.arange(0.5, m, 1)
        Y = np.arange(0.5, n, 1)
        
        # self.action swap 0 <-> 2
        mat = copy.deepcopy(self.action)
        self.action[mat == 0] = 2
        self.action[mat == 2] = 0
        
        U = np.cos(self.action * np.pi / 2)
        V = np.sin(self.action * np.pi / 2)

        plt.figure()
        ax = plt.gca()
        ax.invert_yaxis()
        ax.set_xticks(np.arange(0, 11, 1))
        ax.set_yticks(np.arange(0, 11, 1))
        plt.title("Actions")
        plt.pcolor(self.reward, alpha=0.5, cmap="RdBu")
        plt.colorbar()
        plt.quiver(X, Y, U, V, units='x', pivot='mid')
        plt.grid()
        plt.title(filename)
        plt.savefig(filename+'.png')

    def plot_value(self,filename, text_reveal = True, decimal = 3):
        fig = plt.figure()
        ax = plt.gca()
        ax.invert_yaxis()
        ax.set_xticks(np.arange(0, 11, 1))
        ax.set_yticks(np.arange(0, 11, 1))
        heatmap = plt.pcolor(self.V, cmap="RdBu")
        if (text_reveal == True):
            for y in range(self.V.shape[0]):
                for x in range(self.V.shape[1]):
                    plt.text(x + 0.5, y + 0.5, '%.2f' % self.V[y, x],
                             horizontalalignment='center',
                             verticalalignment='center',
                             fontsize=10,
                             color='white'
                             )
        plt.colorbar()
        plt.grid()
        plt.title(filename)
        plt.savefig(filename+'.png')

def initial_reward_map():
    r_1 = np.zeros([10, 10], dtype="float32")
    r_1[9, 9] = 1
    r_2 = np.zeros([10, 10], dtype="float32")

    list = np.array(
        [[1, 4], [1, 5], [1, 6], [2, 4], [2, 6], [3, 4], [3, 6], [3, 7], [3, 8], [4, 4], [4, 8], [5, 4], [5, 8], [6, 4],
         [6, 8], [7, 6], [7, 7], [7, 8], [8, 6]])
    for ls in list:
        i = ls[0]
        j = ls[1]
        r_2[i, j] = -100
    r_2[9, 9] = 10
    return r_1, r_2

def action_map_return():
    lamb = np.linspace(0, 5, 500)
    r_1, r_2 = initial_reward_map()
    mdp_process = MDP()
    mdp_process.reward = r_1
    mdp_process.delta = 1e10  # should be a very large number
    mdp_process.epsilon = 0.01
    mdp_process.gamma = 0.8
    mdp_process.w = 0.1

    mdp_process.update()
    action_data1 = mdp_process.action

    mdp_process = MDP()
    mdp_process.reward = r_2
    mdp_process.delta = 1e10  # should be a very large number
    mdp_process.epsilon = 0.01
    mdp_process.gamma = 0.8
    mdp_process.w = 0.1

    mdp_process.update()
    action_data2 = mdp_process.action
    return action_data1, action_data2

def get_reward(c, D, b):
    D = matrix(D)
    b = matrix(b)
    c = matrix(c)
    solvers.options['show_progress'] = False
    sol=solvers.lp(c,D,b)
    return sol['x'][-100:]

def reward_max(reward):
    result = []
    for i in range(reward.shape[1]):
        for j in range(reward.shape[0]):
            result.append(reward[j,i])
    return result

def get_c_D_b(coeffi_lambda, M, reward_matrix):
    # cvx constant set-up
    # c = [1, -lambda * 1, 0]
    c = np.ones([100,1])
    c = np.row_stack([c, -coeffi_lambda * np.ones([100,1])])
    c = np.row_stack([c, np.zeros([100,1])])
    c = np.mat(c)

    # D = [0 0 I;0 0 -I;0 -I -I;0 -I I;I 0 M1;0 0 -M1;I 0 -M2;0 0 -M2;I 0 -M3;0 0 -M3]
    Zero1 = np.zeros([100,100])
    Zero2 = np.zeros([100,200])
    Eye = np.eye(100)
    D1 = np.column_stack((Zero2,Eye))
    D2 = np.column_stack((Zero2,-Eye))
    D3 = np.column_stack((Zero1,-Eye))
    D3 = np.column_stack((D3, -Eye))
    D4 = np.column_stack((Zero1,-Eye))
    D4 = np.column_stack((D4, Eye))

    D = np.row_stack((np.row_stack((D1,D2)), np.row_stack((D3,D4))))
    for M_iter in M:
        row = np.column_stack((Eye,Zero1))
        row = np.column_stack((row,-M_iter))  # [I 0 -M]
        D = np.row_stack((D, row))
        row = np.column_stack((Zero2, -M_iter)) # [0 0 -M]
        D = np.row_stack((D,row))
    D = np.mat(D)
    # Rmax
    Rmax = max(abs(np.array(reward_max(reward_matrix))))

    # b
    b1 = np.array([0.0 for _ in range(800)])
    b2 = np.array([Rmax for _ in range(200)])

    b = np.concatenate((b2, b1), axis=0)
    b = np.mat(b).T

    c = -matrix(c)
    b = matrix(b)
    D = matrix(D)
    return c,D,b

def return_new_mdp(reward_map):
    mdp_process = MDP()
    mdp_process.reward = reward_map
    mdp_process.delta = 1e10  # should be a very large number
    mdp_process.epsilon = 0.01
    mdp_process.gamma = 0.8
    mdp_process.w = 0.1
    mdp_process.update()
    return mdp_process

def create_reward_map(array):
    reward_map = np.zeros([10, 10])
    for i in range(len(array)):
        x = int(i % 10)
        y = int(i / 10)
        reward_map[x][y] = array[i]
    return reward_map

def plot_curve(range,acc,title):
    plt.plot(range, acc)
    plt.title(title)
    plt.xlabel('lambda')
    plt.ylabel('accuracy')
    plt.savefig(title + '.png')

def construct_matrix_constant(mdp_process):
    #action_dict = {0: 'right', 1: 'up', 2: 'left', 3: 'down'}
    action_dict = {2: 'right', 1: 'up', 0: 'left', 3: 'down'}
    action_data1 = mdp_process.action
    I = np.mat(np.eye(100), dtype=float)
    Pa1 = np.zeros([100, 100])
    Pa = []
    Pa.append([])
    Pa.append([])
    Pa.append([])
    M = [np.zeros([100, 100])] * 3

    for state_number in range(100):
        agent = 0
        state_x = int(state_number % 10)
        state_y = int(state_number / 10)
        opt_action = action_dict[action_data1[state_x][state_y]]
        for action_iter in ['up', 'down', 'left', 'right']:
            if action_iter != opt_action:
                # print(mdp_process.transition_mat_dict[action_iter][state_number])
                Pa[agent].append(mdp_process.transition_mat_dict[action_iter][state_number])
                agent += 1
            else:
                Pa1[state_number] = mdp_process.transition_mat_dict[action_iter][state_number]
    Pa1 = np.mat(Pa1)
    for i in range(3):
        Pa[i] = np.mat(Pa[i])
        M[i] = (Pa1 - Pa[i]) * ((I - mdp_process.gamma * Pa1).I)
        # M[i] = np.dot((np.array(Pa1) - np.array(Pa[i])),
        #                  np.linalg.inv(np.eye(100) - 0.8 * np.array(Pa1)))
    return M

def plot_heat_map(reward,filename):
    fig = plt.figure()
    ax = plt.gca()
    ax.invert_yaxis()
    ax.set_xticks(np.arange(0, 11, 1))
    ax.set_yticks(np.arange(0, 11, 1))
    plt.pcolor(reward, cmap="RdBu", alpha=0.5)
    plt.colorbar()
    plt.grid()
    plt.title(filename)
    plt.savefig(filename + '.png')

def plot_mesh(reward, filename, zlim):
    """
    3-D plot of reward function
    zlim : zlimit, should be a list [lower-bound, upper-bound]
    """
    [m,n] = reward.shape
    X = np.arange(m)
    Y = np.arange(n)
    X, Y = np.meshgrid(X, Y)
    Z = reward
    
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.set_zlim(zlim[0], zlim[1])
    ax.view_init(azim=220)
    
    # Plot the surface.
    surf = ax.plot_surface(X, Y, Z, cmap="Greens",
                           linewidth=1,
                           antialiased=False)
    
    fig.colorbar(surf, shrink=0.5, aspect=5)
    plt.title(filename)
    plt.savefig(filename + '.png')