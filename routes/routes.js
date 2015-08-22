var express = require('express');
var router = express.Router();
var r_nlp = require('../server/r_nlp.js');

/* GET home page. */
router.get('/', function(req, res, next) {
    res.render('index', { title: 'Express' });
});

/* GET home page. */
router.post('/', function(req, res, next) {
    var input_text = req.body.text;
    r_nlp.process(input_text, function(err, data) {
        if(err) { res.status(500).json({'error':err}) }
        else {
            res.json(data);
        }
    });
});

module.exports = router;
