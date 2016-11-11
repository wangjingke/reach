class Meterplus(object):
    'Meterplus object'
    def read(self, subjectID, wave, rawData):
        def to_sec(time):
            import re
            epoch = 0
            extract = re.search("[0-9]{2}:[0-9]{2}:[0-9]{2}", time)
            if extract:
                string_time = extract.group(0).split(":")
                for i in range(0, len(string_time)):
                    epoch  += 60**int(len(string_time)-1-i)*int(string_time[i])
            return epoch
        def addZero(seg):
            if len(seg) >= 2:
                return seg
            else:
                return "0"+seg

        try:
            file = open(rawData, "r")
            raw = file.readlines()
            file.close()
            
            start_date_seg = raw[3].split("Start Date")[1].strip(" ,\n").split("/")
            start_date = start_date_seg[2]+"-"+addZero(start_date_seg[0])+"-"+addZero(start_date_seg[1])
            
            self.basicInfo = {
                "subjectID": subjectID,
                "wave": wave,
                "age": age(subjectID, start_date),
                "serial": raw[1].split(":")[1].strip(" ,\n"),
                "start_date": start_date,
                "start_time": raw[2].split("Start Time")[1].strip(" ,\n"),
                "epoch": to_sec(raw[4]),
                "file": rawData
            }
        except:
            self.data = None
        else:
            self.data = []
            if 'DataTable' in rawData:
                for i in range(11, len(raw)):
                    self.data.append(int(raw[i].split(",")[2]))
            else:
                for i in range(10, len(raw)):
                    self.data.append(int(raw[i].split(",")[0]))
    def label(self):
        label(self)
    def save(self, filename = 'default.p'):
        save(self, filename)
    def pulse(self, start, end, timezone = "America/Los_Angeles"):
        from dateutil.parser import parse
        import pytz
        LA = pytz.timezone(timezone)
        start_point = LA.localize(parse(start))
        end_point = LA.localize(parse(end))
        df = self.df[(start_point <= self.df['timestamp']) & (self.df['timestamp'] <= end_point)]
        return details(df)
        
def save(meterplus, filename = 'default.p'):
    import pickle
    import gzip
    with gzip.open(filename, 'wb') as f:
        pickle.dump(meterplus, f, protocol = 2)
    
def load(filename):
    import pickle
    import gzip
    with gzip.open(filename, 'rb') as f:
        return pickle.load(f)

def details(df):
    columns = ['nonvalid', 'sed', 'light', 'mod', 'vig']
    details = df['min'].groupby([df['date'], df['mvpa']]).sum().unstack()
    header = list(details.columns.values)
    diff = set(columns) - set(header)
    for i in diff:
        details[i] = 0
    details['total'] = details.sum(axis = 1)
    details['valid'] = details['total'] - details['nonvalid']
    details['total_valid_hr'] = round(details['valid']/60, 2)
    details['valid_day'] = details['total_valid_hr'] >= 10
    return details

def label(meterplus, timezone = "America/Los_Angeles"):
    import numpy as np
    import pandas as pd
    from dateutil.parser import parse
    from datetime import timedelta
    import pytz
    # timestamp conversion
    LA = pytz.timezone(timezone)
    # convert local inital time to UTC for the convenience of calculation
    initial_time_stamp = LA.localize(parse(meterplus.basicInfo['start_date'] + ' ' + meterplus.basicInfo['start_time'])).astimezone(pytz.timezone("UTC"))
    timestamp = []
    date = []
    for i in range(0, len(meterplus.data)):
        # convert UTC back to local time for readability
        timestamp.append((initial_time_stamp + timedelta(seconds = meterplus.basicInfo['epoch']*i)).astimezone(LA))
        date.append(timestamp[i].strftime("%Y-%m-%d"))
    def validate(y):
        counter = 0
        threshold = 3600 / meterplus.basicInfo['epoch']
        valid = []
        for i in meterplus.data:
            if i == 0:
                counter += 1
            else:
                if counter >= threshold:
                    valid.extend([0]*counter)
                else:
                    valid.extend([1]*counter)
                valid.append(1)
                counter = 0
        # add zero for the last continous zeros before ending
        if counter != 0:
            if counter >= threshold:
                valid.extend([0]*counter)
            else:
                valid.extend([1]*counter)
        return valid
    valid = validate(meterplus.data)
    # cutoff for the Troiano criteria
    troiano = {
        "type":["sedentary", "light", "moderate", "vigorous"],
        6:[0, 100, 1400, 3758],
        7:[0, 100, 1515, 3947],
        8:[0, 100, 1638, 4147],
        9:[0, 100, 1770, 4360],
        10:[0, 100, 1910, 4588],
        11:[0, 100, 2059, 4832],
        12:[0, 100, 2220, 5094],
        13:[0, 100, 2393, 5375],
        14:[0, 100, 2580, 5679],
        15:[0, 100, 2781, 6007],
        16:[0, 100, 3000, 6363],
        17:[0, 100, 3239, 6751],
        "adult":[0, 100, 2020, 5999]
    }
    cutoff = list(map(lambda x: x/(60/meterplus.basicInfo['epoch']), troiano[meterplus.basicInfo['age']]))
    def categorize(valid, y):
        if valid==0:
            return 'nonvalid'
        elif valid==1:
            if cutoff[0] <= y < cutoff[1]:
                return 'sed'
            elif cutoff[1] <= y < cutoff[2]:
                return 'light'
            elif cutoff[2] <= y < cutoff[3]:
                return 'mod'
            elif cutoff[3] < y:
                return 'vig'
            else:
                return 'NA'
        else:
            return 'NA'
    mvpa = list(map(categorize, valid, meterplus.data))
    meterplus.df = pd.DataFrame({
        'y': meterplus.data,
        'timestamp': timestamp,
        'date': date,
        'valid': valid,
        'mvpa': mvpa
    })
    meterplus.df['min'] = meterplus.basicInfo['epoch']/60
    meterplus.details = details(meterplus.df)
    def summary(details):
        summary = details.ix[details['valid_day'], ['sed', 'light', 'mod', 'vig', 'valid', 'nonvalid']].mean(axis = 0)
        output = {
            'total_day': len(details['valid_day']),
            'total_valid_day': sum(details['valid_day']),
            'avg_valid': round(float(summary['valid']), 2),
            'avg_nonvalid': round(float(summary['nonvalid']), 2),
            'avg_sed': round(float(summary['sed']), 2),
            'avg_light': round(float(summary['light']), 2),
            'avg_mod': round(float(summary['mod']), 2),
            'avg_vig': round(float(summary['vig']), 2)
        }
        return output
    meterplus.summary = summary(meterplus.details)
    
def age(id, date):
    try:
        subjectID = int(id)
    except:
        return 'adult'
    else:
        if subjectID < 12000:
            return 'adult'
        else:
            from math import floor
            dob = {'colname': ['subjectID', 'gender', 'dob.m', 'dob.d', 'dob.y'],
                12020:[1,4,28,2005],
                12021:[1,12,9,2004],
                12022:[0,4,21,2004],
                12023:[1,5,21,2004],
                12024:[0,12,31,2003],
                12025:[1,4,13,2005],
                12026:[1,10,11,2004],
                12027:[1,2,3,2005],
                12028:[0,9,7,2004],
                12029:[0,10,18,2004],
                12031:[0,8,17,2005],
                12032:[0,2,18,2004],
                12033:[1,9,29,2005],
                12034:[0,10,25,2004],
                12035:[1,8,27,2005],
                12036:[1,6,12,2004],
                12037:[1,4,10,2004],
                12038:[0,5,27,2004],
                12040:[1,3,1,2005],
                12041:[1,3,16,2004],
                12042:[0,6,7,2005],
                12043:[0,3,15,2005],
                12044:[0,6,28,2004],
                12045:[1,10,22,2005],
                12046:[1,1,21,2005],
                12047:[0,4,19,2004],
                12048:[1,9,20,2005],
                12049:[1,3,1,2005],
                12050:[0,10,1,2005],
                12051:[1,3,4,2006],
                12052:[0,3,26,2004],
                12053:[0,12,14,2003],
                12054:[1,5,26,2004],
                12055:[0,6,9,2005],
                12056:[0,12,24,2004],
                12057:[1,12,20,2004],
                12058:[0,11,29,2004],
                12059:[0,6,22,2004],
                12060:[1,4,22,2005],
                12061:[1,5,4,2004],
                12062:[1,11,20,2003],
                12063:[0,7,21,2004],
                12064:[1,3,29,2005],
                12065:[1,6,21,2004],
                12066:[1,9,9,2004],
                12067:[1,9,8,2004],
                12068:[0,5,6,2004],
                12069:[0,10,13,2005],
                12070:[0,4,30,2004],
                12071:[1,2,20,2004],
                12072:[1,9,30,2005],
                12073:[0,4,15,2004],
                12074:[0,8,25,2005],
                12075:[0,6,8,2004],
                12076:[1,5,5,2004],
                12077:[0,3,5,2004],
                12078:[1,7,1,2004],
                12079:[0,5,31,2004],
                12080:[0,11,2,2005],
                12081:[0,4,1,2004],
                12082:[1,4,10,2005],
                12083:[0,2,16,2005],
                12084:[1,1,5,2003],
                12085:[0,7,7,2004],
                12086:[0,2,3,2004],
                12087:[0,12,10,2004],
                12088:[1,5,1,2005],
                12089:[1,8,5,2005],
                12090:[1,9,24,2004],
                12091:[0,6,29,2003],
                12092:[0,3,4,2005],
                12093:[0,6,10,2003],
                12094:[1,8,9,2003],
                12095:[1,10,14,2005],
                12096:[0,1,20,2004],
                12097:[0,4,14,2004],
                12098:[1,12,18,2004],
                12099:[0,2,15,2004],
                12100:[1,5,3,2006],
                12101:[0,9,19,2004],
                12102:[1,3,26,2004],
                12103:[0,2,8,2005],
                12104:[0,7,22,2005],
                12105:[0,5,21,2003],
                12106:[0,5,22,2003],
                12107:[1,12,5,2003],
                12108:[0,11,29,2004],
                12109:[0,2,19,2004],
                12110:[1,8,24,2006],
                12111:[0,2,28,2005],
                12112:[0,2,26,2005],
                12113:[1,2,18,2004],
                12114:[0,10,19,2006],
                12115:[1,12,16,2005],
                12116:[0,6,15,2004],
                12117:[0,8,17,2005],
                12118:[1,8,7,2006],
                12119:[1,11,5,2006],
                12120:[0,7,26,2006],
                12121:[1,10,12,2004],
                12122:[1,6,15,2003],
                12123:[1,5,31,2006],
                12124:[1,2,27,2004],
                12125:[0,11,29,2005],
                12126:[0,4,3,2006],
                12127:[0,9,13,2003],
                12128:[1,9,18,2004],
                12129:[1,6,3,2003],
                12130:[1,7,25,2006],
                12131:[1,11,21,2004],
                12132:[0,4,2,2003],
                12133:[0,8,24,2005],
                12134:[0,11,26,2006],
                12135:[1,3,27,2004],
                12136:[1,9,15,2005],
                12137:[0,8,23,2003],
                12139:[1,3,29,2006],
                12140:[1,11,5,2005],
                12141:[1,6,7,2004],
                12142:[0,10,11,2004],
                12143:[1,3,20,2005],
                12144:[1,5,13,2003],
                12145:[1,11,6,2006],
                12146:[1,6,16,2004],
                12147:[1,10,11,2004],
                12148:[1,10,11,2002],
                12149:[0,3,27,2004],
                12150:[0,12,10,2003],
                12151:[0,6,10,2005],
                12152:[0,9,24,2003],
                12153:[1,11,2,2004],
                12154:[0,11,11,2005],
                12155:[0,5,26,2006],
                12156:[0,5,10,2005],
                12157:[1,3,25,2005],
                12158:[0,4,11,2005],
                12159:[0,10,17,2006],
                12160:[0,8,16,2003],
                12161:[1,5,30,2005],
                12162:[0,7,7,2005],
                12163:[1,10,5,2004],
                12164:[1,4,27,2006],
                12165:[1,12,15,2005],
                12166:[1,6,23,2004],
                12167:[1,2,18,2004],
                12168:[0,6,16,2005],
                12169:[1,11,16,2006],
                12170:[1,7,2,2005],
                12171:[1,3,16,2006],
                12172:[1,10,13,2004],
                12173:[0,5,25,2004],
                12174:[0,7,28,2006],
                12175:[1,9,5,2006],
                12176:[1,5,14,2006],
                12177:[0,6,2,2005],
                12178:[0,1,19,2004],
                12179:[0,10,2,2004],
                12180:[1,3,14,2005],
                12181:[0,9,28,2005],
                12182:[1,7,11,2003],
                12183:[0,9,22,2004],
                12184:[1,10,27,2004],
                12185:[1,7,3,2005],
                12186:[0,8,9,2005],
                12187:[0,9,4,2004],
                12188:[0,7,20,2004],
                12189:[0,9,8,2005],
                12190:[0,2,21,2006],
                12191:[1,5,13,2004],
                12192:[1,8,10,2005],
                12193:[1,3,18,2004],
                12194:[0,8,9,2004],
                12195:[1,10,6,2005],
                12196:[0,4,16,2005],
                12197:[1,3,19,2006],
                12198:[1,3,11,2006],
                12199:[1,10,19,2005],
                12200:[1,7,8,2005],
                12201:[0,1,19,2005],
                12202:[0,12,16,2004],
                12204:[0,11,3,2004],
                12205:[0,3,1,2006],
                12206:[0,7,22,2004],
                12207:[0,6,5,2003],
                12209:[1,11,30,2004],
                12210:[0,3,24,2004],
                12212:[1,7,19,2005],
                12213:[0,5,8,2004],
                12215:[1,8,30,2004],
                12217:[0,5,4,2006],
                12218:[0,5,20,2006],
                12219:[1,2,15,2004],
                12223:[0,6,13,2005],
                12225:[1,2,11,2006],
                12226:[0,3,14,2006],
                12227:[1,9,9,2006],
                12229:[0,1,21,2007],
                12231:[0,7,5,2006],
                12232:[0,1,15,2004],
                12233:[1,5,26,2006],
                12211:[0,6,14,2006],
                12214:[1,7,12,2004]}
            date_seg = list(map(int, date.split('-')))
            age = floor(date_seg[0] - dob[subjectID][3] + (date_seg[1] - dob[subjectID][1])/12 + (date_seg[2] - dob[subjectID][2])/365.25)
            return age