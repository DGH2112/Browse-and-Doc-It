Contributing to Browse and Doc it
=================================

Please try and follows the things that are layed out below as it will make it easier to accept a pull request however not following the below does not necessarily exclude a pull request from being accepted.

## Git Flow

For [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872) I use Git as the version control but I also use [Git Flow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow) for the development cycles. The main development is undertaken in the **Development** branch with stable releases being in the **master**. All pull requests should be made from the **Development** branch, prefereably using **Feature** branches or **BugFix** branches. I've defined prefixes for these already in the `.gitconfig` file. You should submit onyl one change per pull request at a time to make it easiler to review and accept the pull request.

Tools wise, I generally use [SourceTree](https://www.sourcetreeapp.com/) but that does not support Git Flow's **BugFix** functionality so I drop down to the command prompt to create **BugFix** branches as SourceTree can _Finish_ any type of open branch in Git Flow.

## Creating Pull Requests

Having not done this before as I've always been the sole contributor to my repositories so I borrowed the essense of the following from the [DUnitX](https://github.com/VSoftTechnologies/DUnitX) project:

1. Create a GitHub Account (https://github.com/join);
2. Fork the [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872)
   Repository and setup your local repository as follows:
     * Fork the repository (https://help.github.com/articles/fork-a-repo);
     * Clone your Fork to your local machine;
     * Configure upstream remote to the **Development**
       [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872)
       repository (https://github.com/DGH2112/Browse-and-Doc-It);
3. For each change you want to make:
     * Create a new **Feature** or **BugFix** branch for your change;
     * Make your change in your new branch;
     * **Verify code compiles for ALL supported RAD Studio version (see below) and unit tests still pass**;
     * Commit change to your local repository;
     * Push change to your remote repository;
     * Submit a Pull Request (https://help.github.com/articles/using-pull-requests);
     * Note: local and remote branches can be deleted after pull request has been accepted.

**Note:** Getting changes from others requires [Syncing your Local repository](https://help.github.com/articles/syncing-a-fork) with the **Development** [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872) repository. This can happen at any time.

## Dependencies

[Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872) has 2 dependencies as follows:

* Virtualtrees - This is implemented as a sub-module and it modified to allow IDE theming (https://github.com/DGH2112/Virtual-Treeview-6.5.0-IDE);
* SynEdit 2.1.0 - this is ONLY requested to compile and run the test projects which allows you to parse directories of code for testing.

## Project Configuration

The [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872) Open Tools API project uses a single projects file (`.DPR`) to compile to mutliple versions of RAD Studio by use 2 include files: one for compiler specific coding and the second to implement the correct suffix for the DLL.

The current code base only supports RAD Studio XE3 and above. The limitation here is the version of VirtualTrees (V6.5). To supports earlier version I would need to either support VTV V5.3 and `IFDEF` loads of stuff or modifiy the VTV V6.5 code to be more backwards compatible which I know is possible as Stefan Gleinke has done this, I just haven't had time.

Note: The web page is out of date with regards to the supported versions above.

## Rationale

The following is a brief description of the rationale behind [Browse and Doc It](https://www.davidghoyle.co.uk/WordPress/?page_id=872). I will hopefully write more later.

At the root of everything is a class called `TElementContainer`. This class forms the basic building structure for every documentable / browseable item in the code. Modules are specialised classes which contain a hierarchy of these objects to form a tree of the parsed code. The `TElementContainer` class can contain any number of other `TElementContainer` classes and each `TElementContainer` class can contain any number of Tokens that represent what that item is. Other specialist classes, for instance, methods, have specific properties for those classes, i.e. parameters, return types, etc. Each element can have a `TComment` associated with it in which the documentation is retreived. Each parser may implement a specialisation of the `TComment` class. The `TComment` class does the current parsing of the Java Doc type comments excluding the lagunage specific comment prolog / epilog characters. The `TComment` specialisations are responsible for extracting the comment text (without prolog and epilog code) from the source code.

Most parsers were written around are 2 stage process: the first stage tokenises the input stream and the second parses the token stream. When I first started this I know nothing about parsing code and these parsers are the 3rd generation of parsers I've implemented. I literally stumbled upon the recursive descent algorthym during the development which I now know is generally the preferred method for parsing. If I would to write a new parser for something then I might change to a single process which tokenises the the stream on the fly as the code is being parsed - this was you can better understand context.

The module explorer and documentation engine are designed to render the modules and their hierarchical contents.

There are reams more I could and should write but for now the above will have to do. When I get around to releasing the binaries for version 1.3a I will attempt to include some class diagrams for the code to help you understand it better. If there is something you do not understand (and I sometimes have to think hard if I've not touched it for years) then please ask before diving in.

regards

David Hoyle Jun 2019.