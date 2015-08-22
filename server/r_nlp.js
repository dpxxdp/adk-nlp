var spawn = require('child_process').spawn;
var env = process.env;

var async = require('async');
var queue = async.queue;

var opts = { cwd: './Rwork',
    env: process.env
};
// can modify child env with needed key-value pairs
opts.env['YEAR']=2013;
opts.env['BASIN']='SC';
var RCall = ['--no-restore','--no-save','my_R_script.R'];
var R  = spawn('Rscript', RCall, opts);

var jobs = 4;


function setup_R_job(opts,callback){

    var R  = spawn('Rscript', RCall, opts);
    R.on('exit',function(code){
        console.log('got exit code: '+ code);
        if(code==1){
            callback(err);
        }else{
            callback(null, data);
        }
        return null
    });
    return null
}

exports.process = function(input_text, callback) {

    var basin_queue=queue(setup_R_job, jobs);

    var opts = { cwd: './r/nlp',
        env: process.env
    };

    var years = [2007,2008,2009,2010,2011,2012,2013];
    var airbasins = [ 'GBV', 'MD', 'NEP', 'SC', 'SF', 'LC', 'MC',
        'LT',  'NCC',  'NC',  'SCC', 'SD',  'SJV',
        'SS',  'SV' ];

    _.each(years,function(year){
        _.each(airbasins,function(basin){
            var o = _.clone(opts,true);
            o.env['YEAR'] = year;
            o.env['AIRBASIN'] = basin;
            basin_queue.push(o)
        })
    });

};

