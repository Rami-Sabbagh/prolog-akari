import * as fs from 'fs';
import * as path from 'path';

import { fetchLevel, PuzzleNotFoundError } from "./api";

if (!fs.existsSync('./data')) fs.mkdirSync('./data');

const dimensions = [20, 8, 10, 12, 15];

let state = {
    delay: 5_000,
    dimensionIndex: 0,
    volume: 1,
    book: 1,
    puzzle: 1,
};

if (fs.existsSync('./scraper_state.json')) state = JSON.parse(fs.readFileSync('./scraper_state.json', 'utf8'));

function saveState() {
    fs.writeFileSync('./scraper_state.json', JSON.stringify(state, null, 4));
}

async function delay(time: number) {
    return new Promise(resolve => setTimeout(resolve, time));
}

async function main() {
    console.info(new Date(), '🔔 Job started');
    for (; state.dimensionIndex < dimensions.length; state.dimensionIndex++) {
        const dimension = dimensions[state.dimensionIndex];
        
        for (; state.volume <= 2; state.volume++) {
            for (; state.book <= 100; state.book++) {
                const maxPuzzles = (dimension < 15) ? 24 : 16;

                for (; state.puzzle <= maxPuzzles; state.puzzle++) {
                    saveState();
                    console.log(new Date(), `📄 ${dimension}x${dimension} puzzle #${state.puzzle} from Vol.${state.volume}, Book no.${state.book}`);
                    await delay(state.delay);
                    
                    try {
                        const puzzle = await fetchLevel(dimension, state.puzzle, state.book, state.volume);

                        const filename = `${dimension}x${dimension}_V${state.volume}-B${state.book}-P${state.puzzle}.json`;
                        fs.writeFileSync(path.join('./data', filename), JSON.stringify(puzzle, null, 4));
                    } catch (error) {
                        console.error(new Date(), `💥 ${(error instanceof Error) ? error.message : error}`);
                        if (error instanceof PuzzleNotFoundError) break
                        else state.puzzle--;
                    }
                }
                state.puzzle = 1;
            }
            state.book = 1;
        }
        state.volume = 1;
    }
    saveState();
    console.info(new Date(), '🔔 Job finished');
}

main().catch(console.error);