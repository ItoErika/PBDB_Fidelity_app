# via http://stackoverflow.com/a/21189044/1956065
function parse_yaml {
   local prefix=$2
   local s='[[:space:]]*' w='[a-zA-Z0-9_]*' fs=$(echo @|tr @ '\034')
   sed -ne "s|^\($s\):|\1|" \
        -e "s|^\($s\)\($w\)$s:$s[\"']\(.*\)[\"']$s\$|\1$fs\2$fs\3|p" \
        -e "s|^\($s\)\($w\)$s:$s\(.*\)$s\$|\1$fs\2$fs\3|p"  $1 |
   awk -F$fs '{
      indent = length($1)/2;
      vname[indent] = $2;
      for (i in vname) {if (i > indent) {delete vname[i]}}
      if (length($3) > 0) {
         vn=""; for (i=0; i<indent; i++) {vn=(vn)(vname[i])("_")}
         printf("%s%s%s=\"%s\"\n", "'$prefix'",vn, $2, $3);
      }
   }'
}

eval $(parse_yaml credentials.yml)
eval $(parse_yaml config)

pwd=$(pwd)

# Create the database - if it exists an error will be thrown which can be ignored
createdb $postgres__database -h $postgres__host -U $postgres__user -p $postgres__port

# tuples
echo "DROP TABLE IF EXISTS doc_terms; CREATE TABLE doc_terms (docid text, term text);" | psql -U $postgres__user -h $postgres__host -p $postgres__port $postgres__database

echo "COPY doc_terms FROM '$pwd/input/strat_overlap_doc_terms'" | psql -U $postgres__user -h $postgres__host -p $postgres__port $postgres__database

# NLP352
echo "DROP TABLE IF EXISTS nlp_sentences_352; CREATE TABLE nlp_sentences_352 (docid text, sentid integer, wordidx integer[], words text[], poses text[], ners text[], lemmas text[], dep_paths text[], dep_parents integer[]);" | psql -U $postgres__user -h $postgres__host -p $postgres__port $postgres__database

echo "COPY nlp_sentences_352 FROM '$pwd/input/sentences_nlp352'" | psql -U $postgres__user -h $postgres__host -p $postgres__port $postgres__database
