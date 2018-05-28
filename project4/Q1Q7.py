import numpy as np
import re
import time
import os
os.chdir("/home/kgicmd/pj4")
import multiprocessing as mp
from tqdm import tqdm
import pickle

def read_acts(file_name, famous=False):
    """
    read actors and actress list
    Input: file_name:;
           famous: number of films that actor/actress in
    Output: list of actor/actress and movies
    """
    data = []
    
    f = open(file_name,'r', encoding="ISO-8859-1")  # use this encoding
    for line in f.readlines():
        line = line.strip('\n')
        line = line.replace('\t\t','\t')
        line = line.split('\t')
        if (famous == True):
            if (len(line) > 10):
                data.append(line)
        else:
            data.append(line)
    f.close()
    
    return data

# question 1
    
# original
actor_list = read_acts('actor_movies.txt')
actress_list = read_acts('actress_movies.txt')
num_acts = len(actor_list) + len(actress_list)
print("total number of actors/actresses: {}".format(num_acts))

# famous actors
actor_list_fam = read_acts('actor_movies.txt', famous=True)
actress_list_fam = read_acts('actress_movies.txt', famous=True)
num_acts_fam = len(actor_list_fam) + len(actress_list_fam)
print("total number of famous actors/actresses: {}".format(num_acts_fam))

def clean_data(act_list, verbose=False):
    """
    clean the data of actor/actress list
    Input: actor/actress film list
    Output: actor/actress with cleaned film list
    """
    new_act_list = []
    for ind in range(len(act_list)):
        act_list_line = act_list[ind].copy()
        act_list_line_clean = []
        # add actor name
        act_list_line_clean.append(act_list_line[0])
        
        for i in np.arange(1,len(act_list_line)):
            pattern = r'\(.*?\)' # find everythinf in ()
            res = re.findall(pattern, act_list_line[i])
            
            for j in range(len(res)):
                if(len(res[j]) == 6): # year name
                    pass
                else:
                    act_list_line[i] = act_list_line[i].replace(res[j],'')
            act_list_line[i] = act_list_line[i].replace('{{SUSPENDED}}','')
            if(act_list_line[i] in act_list_line_clean):
                pass
            elif(act_list[i] == ''):
                pass
            else:
                act_list_line[i] = act_list_line[i].rstrip(' ')
                act_list_line_clean.append(act_list_line[i])
        # add cleaned lines
        new_act_list.append(act_list_line_clean)
        
        if(verbose):
            if(ind % 10000 == 0):
                print("processed {}/{}".format(ind, len(act_list)))
    return(new_act_list)

actor_list_fam_clean = clean_data(actor_list_fam, verbose=True)
actress_list_fam_clean = clean_data(actress_list_fam, verbose=True)

# merge actor and actress
merged_list = []
merged_list.extend(actor_list_fam_clean)
merged_list.extend(actress_list_fam_clean)

# choose actors in more than 10 movies
merged_list_famous = []
for i in range(len(merged_list)):
    if(len(merged_list[i]) > 10):
        merged_list_famous.append(merged_list[i])

# number of movies
movie_list = []
for i in range(len(merged_list_famous)):
    movie_list.extend(merged_list_famous[i][1:])

uq_list = np.unique(movie_list)
#sorted_movie_list = sorted(movie_list,key = str.lower)
print("number of unique movies:{}".format(len(uq_list)))

def gen_actor_id(data_list, file_name):
    
    f = open(file_name,'w', encoding="ISO-8859-1")
    for i in range(len(merged_list_famous)):
        act_name = merged_list_famous[i][0]
        act_id = i
        line = act_name + '\t' + str(act_id) + '\n'
        f.write(line)
    
    f.close()
    
    return 0

gen_actor_id(merged_list_famous, "actor_id.txt")

####################################
#=============Question 2============
####################################

# this process is very slow...
f = open('edge_list.txt','w', encoding="ISO-8859-1")
start_time = time.time()
for i in range(len(merged_list_famous)):
    for j in range(i+1,len(merged_list_famous)):
        union_set = set(merged_list_famous[i])& set(merged_list_famous[j])
        card_union = len(union_set)
        
        if(card_union != 0):
            # write to edge_list file
            weight1 = card_union / (len(merged_list_famous[i]) - 1)# i -> j
            weight2 = card_union / (len(merged_list_famous[j]) - 1)# j -> i
            
            line1 = merged_list_famous[i][0] + '\t' \
                    + merged_list_famous[j][0] + '\t' \
                    + str(weight1) + '\n'
                    
            line2 = merged_list_famous[j][0] + '\t' \
                    + merged_list_famous[i][0] + '\t' \
                    + str(weight2) + '\n'
            
            f.write(line1)
            f.write(line2)
    
    if(i % 100 == 0):
        print(i)
        elapsed_time = time.time() - start_time
        len_list = len(merged_list_famous)
        eta = 0
        if (i != 0):
            eta = elapsed_time * len_list * len_list / (2*len_list - i)/i - elapsed_time
            print("time elpasd:{} s, ETA:{}".format(elapsed_time, eta))
        

f.close()

### Question 7
uq_mv_list = uq_list.copy()
def process_list(ind):
    uq_mv_list_sub = []
    movie = uq_mv_list[ind] # get movie name
    if (len(movie) == 0):
        return 0
    else:
        uq_mv_list_sub.append(movie) # insert movie name
        for j in range(len(merged_list_famous)): # insert actor name, use famous actors
            if(movie in merged_list_famous[j]):
                uq_mv_list_sub.append(merged_list_famous[j][0])
        return(uq_mv_list_sub) 

def multicore(func):
    result = []
    pool = mp.Pool(processes=6)
    for y in tqdm(pool.imap(func, np.arange(len(uq_mv_list)))):
        result.append(y)
    return(result)
mv_actor_list = multicore(process_list)

# trim to > 5 actors
mv_actor_list_trim = []
for i in range(len(mv_actor_list)):
    if(mv_actor_list[i] != 0):
        if (len(mv_actor_list[i]) > 5):
            mv_actor_list_trim.append(mv_actor_list[i])

pickle.dump(mv_actor_list_trim, open('/home/kgicmd/pj4/mv_actor_list_trim.pkl', 'wb'))
pickle.dump(mv_actor_list, open('/home/kgicmd/pj4/mv_actor_list.pkl', 'wb'))
