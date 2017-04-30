from riotwatcher import RiotWatcher
from riotwatcher import KOREA
import riotwatcher
import time

w = RiotWatcher("RGAPI-0559ced4-bb1a-4b85-a086-bdb9098f3e14")

success_sample = 0
k = 100000
p = ["", "", "", "", "", "", "", "", "", ""]

dat = open("new_game_data_2.txt", "a")
header = "matchID, matchType, matchMode, queueType, playerID, teamID, Winner, championID, lane, role, spell_1, " \
         "spell_2, tier, K, D, A, dam, dToC, lvl, damToken, heal, minionKill, neutralMinionsKilled, " \
         "goldEarned, totalTimeCrowdControlDealt, gameLength" + "\n"
dat.write(header)

while True:
    print("This is the " + str(k) + " try\n")
    try:
        g = w.get_match(2778000000-k, region=KOREA)
    except riotwatcher.riotwatcher.LoLException as error:
        if error.error == "Game data not found":
            print(error.error)
            k += 1
            continue
        elif error.error == "Too many requests":
            print(error.error)
            time.sleep(8)
            continue
    if g["queueType"] != "TEAM_BUILDER_RANKED_SOLO":
        print("This is not a rank game")
        k += 1
        continue
    success_sample += 1
    k += 1
    for i in range(0, 10, 1):
        p[i] = str(g["matchId"]) + ", " + str(g["matchType"]) + ", " \
               + str(g["matchMode"]) + ", " + str(g["queueType"]) + ", " \
               + str(g["participants"][i]["participantId"]) + ", " \
               + str(g["participants"][i]["teamId"]) + ", " \
               + str(g["participants"][i]["stats"]["winner"]) + ", " \
               + str(g["participants"][i]["championId"]) + ", " \
               + str(g["participants"][i]["timeline"]["lane"]) + ", " \
               + str(g["participants"][i]["timeline"]["role"]) + ", " \
               + str(g["participants"][i]["spell1Id"]) + ", " \
               + str(g["participants"][i]["spell2Id"]) + ", " \
               + str(g["participants"][i]["highestAchievedSeasonTier"]) + ", " \
               + str(g["participants"][i]["stats"]["kills"]) + ", " \
               + str(g["participants"][i]["stats"]["deaths"]) + ", " \
               + str(g["participants"][i]["stats"]["assists"]) + ", " \
               + str(g["participants"][i]["stats"]["totalDamageDealt"]) + ", " \
               + str(g["participants"][i]["stats"]["totalDamageDealtToChampions"]) + ", " \
               + str(g["participants"][i]["stats"]["champLevel"]) + ", " \
               + str(g["participants"][i]["stats"]["totalDamageTaken"]) + ", " \
               + str(g["participants"][i]["stats"]["totalHeal"]) + ", " \
               + str(g["participants"][i]["stats"]["minionsKilled"]) + ", " \
               + str(g["participants"][i]["stats"]["neutralMinionsKilled"]) + ", " \
               + str(g["participants"][i]["stats"]["goldEarned"]) + ", " \
               + str(g["participants"][i]["stats"]["totalTimeCrowdControlDealt"]) + ", " \
               + str(g["matchDuration"]) \
               + "\n"
    dat0 = p[0] + p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9]
    dat.write(dat0)
    print("Now we have " + str(success_sample) + " samples.\n")
    if success_sample == 12000:
        print("We have gotten enough samples!")
        break

dat.close()