import * as fs from 'fs';

import { PuzzleData } from './api';

interface PuzzleID {
    dimension: number;
    volume: number;
    book: number;
    puzzle: number;
}

function parseFilename(filename: string): PuzzleID {
    const match = filename.match(/^(?<dim>\d+)x\d+_V(?<vol>\d+)-B(?<book>\d+)-P(?<puzzle>\d+)\.json$/);

    return {
        dimension: Number.parseInt(match?.groups?.dim ?? '0'),
        volume: Number.parseInt(match?.groups?.vol ?? '0'),
        book: Number.parseInt(match?.groups?.book ?? '0'),
        puzzle: Number.parseInt(match?.groups?.puzzle ?? '0'),
    };
}

function puzzlesCompare(a: string, b: string): number {
    const aID = parseFilename(a), bID = parseFilename(b);
    return (aID.dimension - bID.dimension) || (aID.volume - bID.volume) || (aID.book - bID.book) || (aID.puzzle - bID.puzzle);
}

const packedFile = fs.createWriteStream('./packed-data.csv');

for (const filename of fs.readdirSync('./data').sort(puzzlesCompare)) {
    const puzzle: PuzzleData = JSON.parse(fs.readFileSync(`./data/${filename}`, 'utf8'));
    packedFile.write(`${puzzle.ptitle},${puzzle.width},${puzzle.height},${puzzle.work},${puzzle.puzz},${puzzle.solved}\n`);
}

packedFile.close();
