require 'sinatra'
require 'json'
require 'bson'
require 'grit'

BASE_DIR = ENV["BASE_DIR"]
USERNAME = ENV["USERNAME"]
PASSWORD = ENV["PASSWORD"]

configure do
  set :show_exceptions, false
end

def appendGit(repoName)
  repoName.chomp(File.extname(repoName)) + ".git"
end

def with_repo(userName,repoName)
  repoGitName = appendGit repoName
  repo = Grit::Repo.new(File.join([BASE_DIR,userName,repoGitName]))
  yield(repo)
end

def toSON(x)
  if request.accept.first == 'application/bson'
    content_type 'application/bson'
    BSON.serialize({ :data => JSON.parse(x.to_json)}).to_s
  else
    content_type :json
    x.to_json
  end
end

## Authorization

use Rack::Auth::Basic do |username, password|
  username == USERNAME && password == PASSWORD
end

## Creating a new repo

post "/repos/:user/:repo" do
  repo = appendGit params[:repo]
  Grit::Repo.init_bare(File.join([BASE_DIR,params[:user],repo]))
end

## Branches

get "/repos/:user/:repo/branches" do
  with_repo params[:user], params[:repo] do |repo|
    branches = repo.branches.map do |branch|
      {
        name: branch.name,
        commit: {
          sha: branch.commit.id
        }
      }
    end
    toSON branches
  end
end


## Tags

get "/repos/:user/:repo/tags" do
  with_repo params[:user], params[:repo] do |repo|
    tags = repo.tags.map do |tag|
      {
        name: tag.name,
        commit: {
          sha: tag.commit.id
        }
      }
    end
    toSON tags
  end
end

## Getting a tag

get "/repos/:user/:repo/git/tags/:sha" do
  tags = with_repo params[:user], params[:repo] do |repo|
    tag = repo.tags.find(params[:sha]).first
    { tag: tag.name,
      sha: tag.commit.id,
      message: tag.message,
      tagger: {
          name: tag.tagger.name,
          email: tag.tagger.email,
          date: tag.tag_date
      },
#TODO: object { type, sha }
    }
  end
  toSON tags
end

## Blobs

get "/repos/:user/:repo/git/blobs/:sha" do
  blobs = with_repo params[:user], params[:repo] do |repo|
    blob = repo.blob(params[:sha])
    { 
      content: blob.data,
      mime_type: blob.mime_type
    }
  end
  toSON blobs
end

## Commits

get "/repos/:user/:repo/git/commits/:sha" do
  commit = with_repo params[:user], params[:repo] do |repo|
    commit = repo.commit(params[:sha])
    {
      sha: commit.id,
      author: {
        date: commit.authored_date.xmlschema,
        name: commit.author.name,
        email: commit.author.email
       },
      committer: {
        date: commit.committed_date.xmlschema,
        name: commit.committer.name,
        email: commit.committer.email
      },
      message: commit.message,
      tree: {
          sha: commit.tree.id
      },
      parents: commit.parents.map { |p| { sha: p.id } }
    }
  end
  toSON commit
end

## References

get "/repos/:user/:repo/git/refs" do
  refs = with_repo params[:user], params[:repo] do |repo|
    repo.refs.map do |ref|
      type = ref.class.name.split("::").last.downcase
      {
        ref: "refs/#{type}s/#{ref.name}",
        object: {
          sha: ref.commit,
          type: (type == "tag") ? "tag" : "commit"
        }
      }
    end
  end
  toSON refs
end


# Handle e.g., /repos/:user/:repo/git/refs/heads/master
def get_reference(repo, ref_name) 
  ref = repo.refs.select {|ref|
   ref_name ==
    "#{ref.class.name.split("::").last.downcase}s/#{ref.name}"}.first
  type = ref.class.name.split("::").last.downcase
# for compatability with github, return array:
  [{
    ref: "refs/#{type}s/#{ref.name}",
    object: {
      sha: ref.commit,
      type: (type == "tag") ? "tag" : "commit"
    }
  }]
end

# Handle e.g., /repos/:user/:repo/git/refs/tags
def get_sub_namespace_references(repo, ref_name) 
  refs = repo.refs.select {|ref|
    ref_name == "#{ref.class.name.split("::").last.downcase}s"}
  refs.map do |ref|
    type = ref.class.name.split("::").last.downcase
    {
      ref: "refs/#{type}s/#{ref.name}",
      object: {
        sha: ref.commit,
        type: (type == "tag") ? "tag" : "commit"
      }
    }
  end
end


get "/repos/:user/:repo/git/refs/*" do
  ref_name = params[:splat].first
  refs = with_repo params[:user], params[:repo] do |repo|
      if ref_name.split('/').length > 1
        get_reference repo, ref_name
      else
        get_sub_namespace_references repo, ref_name
      end
  end
  toSON refs
end

## Trees

def makeTree(t)
  {
    path: t.name,
    mode: t.mode,
    type: "tree",
    sha: t.id
  }
end

def makeBlob(t)
  {
    path: t.name,
    mode: t.mode,
    type: "blob",
    size: t.size,
    sha: t.id
  }
end

get "/repos/:user/:repo/git/trees/:sha" do
  tree = with_repo params[:user], params[:repo] do |repo|
    tree = repo.tree(params[:sha])
    sub_trees = tree.trees.map {|t| makeTree(t)}
    blobs = tree.blobs.map {|t| makeBlob(t)}
    {
      sha: tree.id,
      tree: sub_trees + blobs
    }
  end
  toSON tree
end

