var express = require('express');
var router = express.Router();

router.get('/', index);

function index(req, res) {
    res.send({data: "This is from the server!"});
}

module.exports = router;
