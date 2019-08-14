window.onload = function () {
    // console.log("hello");
    // console.log($.getJSON)
    // console.log(document.getElementById("hn-body"));
    // document.getElementById("hn-body").innerHTML = "hello"

    // j = require("assets/starred.json");

    all_tags = ["pl", "ai", "hw", "space", "os"]
    tag2color = function (tag) {
        switch (tag) {
        case "pl": return "lightblue"; break;
        case "ai": return "lightpink"; break;
        case "space": return "yellow"; break;
        case "hw": return "gold"; break;
        case "os": return "lightgreen"; break;
        default: return "white";
        }
    }

    add_article = function (j) {
        // var newdiv = document.createElement("div");
        // time
        newdiv = $("<div></div>").attr("id", j["id"]).addClass("hn-article")
        newdiv.append($("<span></span>").addClass("hn-time")
                      .text(new Date(j["time"] * 1000).toLocaleDateString().padEnd(30, " ")))
        // comment
        newdiv.append(
            $("<span></span>").addClass("hn-comment")
                .append($("<a></a>")
                        .attr("href", "https://news.ycombinator.com/item?id=" + j["id"])
                        .attr("target", "_blank")
                        .text(j["descendants"])))
        // score
        newdiv.append($("<span></span>").addClass("hn-score").text(j["score"]))
        // title
        newdiv.append($("<a></a>").addClass("hn-title")
                      .attr("href", j["url"])
                      .attr("target", "_blank")
                      .text(j["title"]))
        // user
        // newdiv.append($("<span></span>").addClass("hn-by").text(j["by"]))
        // tags
        tagspan = $("<span></span>").addClass("hn-tags")
        tags = j["tags"]
        if (!tags) tags = []
        // tags = tags.concat(all_tags)
        for (t of tags) {
            tagspan.append($("<span></span>").text(t)
                           .attr("style", "background-color: " + tag2color(t)))
        }
        newdiv.append(tagspan)
        $("#hn-body").append(newdiv);
    }

    $.getJSON("assets/hn.json", function(json) {
        $("#hn-body").append("<p>Total articles: " + json.length + ", Order: newest first</p>")
        
        header = {"id": "id",
                  // "time": new Date() / 1000,
                  "time": 1 * 3600 * 24,
                  "url": "url",
                  "title": "title",
                  "score": "score",
                  "descendants": "#cmts",
                  "by": "user",
                  "tags": ["tags: "].concat(all_tags)
                 }
        add_article(header)

        $("#hn-body").append("<hr/>")

        // sort by date
        json.sort(function (a, b) {
            return b["time"] - a["time"];
        })
        
        for (j of json) {
            add_article(j)
        }
    });
}
