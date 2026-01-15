using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualBasic.FileIO;
using Occult.Star_Catalogues;

namespace Occult.File_Actions
{
	public class DeleteSupercededFiles : Form
	{
		private string FilesForDeletionDirectory = "";

		private string GaiaSourceDirectory = "";

		private string KMZSourceDirectory = "";

		private string PredictionSourceDirectory = "";

		private string ObservationSourceDirectory = "";

		private string DownloadSourceDirectory = "";

		private string ResourceSourceDirectory = "";

		private string FilesForDeletionDirectory_Gaia = "";

		private string FilesForDeletionDirectory_KMZ = "";

		private string FilesForDeletionDirectory_Predict = "";

		private string FilesForDeletionDirectory_Observe = "";

		private string FilesForDeletionDirectory_Download = "";

		private string FilesForDeletionDirectory_Resource = "";

		private static long GaiaLength;

		private static long KMZlength;

		private static long PredictionLength;

		private static long ObservationLength;

		private static long DownloadLength;

		private static long ResourceLength;

		private List<FileNameDate> ExtraGaia;

		private List<FileNameDate> KMZfiles;

		private List<FileNameDate> PredictionFiles;

		private List<FileNameDate> ObservationFiles;

		private List<FileNameDate> DownloadFiles;

		private List<FileNameDate> ExtraResourceFiles;

		private FileNameDate EGaia;

		private FileNameDate GEkmz;

		private FileNameDate Predn;

		private FileNameDate Obsn;

		private FileNameDate Download;

		private FileNameDate EResource;

		private DateTime FilterDateKMZ;

		private DateTime FilterDatePredict;

		private DateTime FilterDateObserve;

		private DateTime FilterDateDownload;

		private DateTime FilterDateResource;

		private static DisplayData DisplayObservations;

		private static DisplayData DisplayResources;

		private static DisplayData DisplayPrediction;

		private IContainer components;

		private CheckedListBox chkGaiaExtras;

		private ListBox lstGaiaImportant;

		private Label label1;

		private Button cmdSetAllGaiaChecks;

		private Button cmdClearGaiaChecks;

		private Label label2;

		private Label label3;

		private Button cmdMoveGaia;

		private Label lblMoveNote;

		private Button cmdMoveKMZ;

		private Button cmdClearKMZ;

		private Button cmdSetAllKMZ;

		private CheckedListBox chkKMZ;

		private Label label4;

		private NumericUpDown updnMonthKmz;

		private NumericUpDown updnYearKmz;

		private Label label5;

		private TabControl TabMoveDelete;

		private TabPage tabPage1;

		private TabPage tabPage2;

		private TabPage tabPage3;

		private NumericUpDown updnMonthPredict;

		private NumericUpDown updnYearPredict;

		private Label label6;

		private Label label7;

		private Button cmdMovePredictions;

		private CheckedListBox chkPredictions;

		private Button cmdClearAllPredictions;

		private Button cmdSetAllPredictions;

		private Button cmdDeleteGaia;

		private Button cmdDeleteKMZ;

		private Button cmdDeletePredictions;

		private TabPage tabPage4;

		private Button cmdDeleteObservations;

		private NumericUpDown updnMonthObservation;

		private NumericUpDown updnYearObservation;

		private Label label8;

		private Label label9;

		private Button cmdMoveObservations;

		private CheckedListBox chkObservations;

		private Button cmdClearObservations;

		private Button cmdSetObservations;

		private Label label11;

		private TextBox txtNameContainsObs;

		private Label label10;

		private Label label14;

		private TextBox txtNameContainsKMZ;

		private Label label15;

		private Label label12;

		private TextBox txtNameContainsPredict;

		private Label label13;

		private Label label16;

		private TabPage tabPage5;

		private Label label17;

		private TextBox txtNameContainsDownload;

		private Label label18;

		private Button cmdDeleteDownload;

		private NumericUpDown updnMonthDownload;

		private NumericUpDown updnYearDownload;

		private Label label19;

		private Label label20;

		private Button cmdMoveDownload;

		private CheckedListBox chkDownload;

		private Button cmdClearDownload;

		private Button cmdSetDownload;

		private ListBox lstDownloadReserved;

		private Label label21;

		private Label lblKMZsize;

		private Label lblGaiasize;

		private Label lblPredictsize;

		private Label lblObservesize;

		private Label lblDownloadsize;

		private CheckBox chkMap;

		private TabPage tabPage6;

		private Label lblResourceSize;

		private Button cmdDeleteResources;

		private Button cmdMoveResources;

		private Label label23;

		private Label label24;

		private Label label25;

		private CheckedListBox chkResources_Extra;

		private Button cmdClearAllResourceChecks;

		private ListBox lstResourceReserved;

		private Button cmdSetAllResourceChecks;

		private Label label22;

		private TextBox txtNameContainsResource;

		private Label label26;

		private NumericUpDown updnMonthResource;

		private NumericUpDown updnYearResource;

		private Label label27;

		private Label lblReservedResourceFilesSize;

		private Label label28;

		private Button cmdDisplayResources;

		private Button DisplayDownloads;

		private Button cmdDisplayPredictions;

		private Button cmdDisplayObservations;

		private Button cmdDisplayKMZ;

		public DeleteSupercededFiles()
		{
			InitializeComponent();
		}

		private void DeleteSupercededFiles_Load(object sender, EventArgs e)
		{
			FilesForDeletionDirectory = Utilities.AppPath + "\\FilesForDeletion";
			FilesForDeletionDirectory_Gaia = FilesForDeletionDirectory + "\\Gaia";
			FilesForDeletionDirectory_KMZ = FilesForDeletionDirectory + "\\kmz";
			FilesForDeletionDirectory_Predict = FilesForDeletionDirectory + "\\Prediction";
			FilesForDeletionDirectory_Observe = FilesForDeletionDirectory + "\\Observation";
			FilesForDeletionDirectory_Download = FilesForDeletionDirectory + "\\Download";
			FilesForDeletionDirectory_Resource = FilesForDeletionDirectory + "\\ResourceFiles";
			GaiaSourceDirectory = Utilities.AppPath + "\\Resource Files\\Gaia";
			KMZSourceDirectory = Utilities.AppPath + "\\Generated Files\\GoogleEarth";
			PredictionSourceDirectory = Utilities.AppPath + "\\Predictions";
			ObservationSourceDirectory = Utilities.AppPath + "\\Observations";
			DownloadSourceDirectory = Utilities.AppPath + "\\Downloaded Files";
			ResourceSourceDirectory = Utilities.AppPath + "\\Resource Files";
			((Control)lblMoveNote).set_Text("Files set to 'For Deletion' are moved to the directory  " + FilesForDeletionDirectory + "\r\nfrom where they can be recovered. Use your usual file manager to delete files from that directory");
			((Control)lblMoveNote).set_Left((((Control)this).get_Width() - ((Control)lblMoveNote).get_Width()) / 2);
			PopulateGaiaFiles();
			updnMonthKmz.set_Value((decimal)DateTime.Now.Month);
			updnYearKmz.set_Value((decimal)DateTime.Now.Year);
			FilterDateKMZ = new DateTime((int)updnYearKmz.get_Value(), (int)updnMonthKmz.get_Value(), 1);
			PopulateKMZfiles();
			updnMonthPredict.set_Value((decimal)DateTime.Now.Month);
			updnYearPredict.set_Value((decimal)DateTime.Now.Year);
			FilterDateKMZ = new DateTime((int)updnYearPredict.get_Value(), (int)updnMonthPredict.get_Value(), 1);
			PopulatePredictionFiles();
			updnMonthObservation.set_Value((decimal)DateTime.Now.Month);
			updnYearObservation.set_Value((decimal)DateTime.Now.Year);
			FilterDateObserve = new DateTime((int)updnYearObservation.get_Value(), (int)updnMonthObservation.get_Value(), 1);
			PopulatePredictionFiles();
			updnMonthDownload.set_Value((decimal)DateTime.Now.Month);
			updnYearDownload.set_Value((decimal)DateTime.Now.Year);
			FilterDateDownload = new DateTime((int)updnYearDownload.get_Value(), (int)updnMonthDownload.get_Value(), 1);
			PopulateDownloadFiles();
			updnMonthResource.set_Value((decimal)DateTime.Now.Month);
			updnYearResource.set_Value((decimal)DateTime.Now.Year);
			FilterDateResource = new DateTime((int)updnYearResource.get_Value(), (int)updnMonthResource.get_Value(), 1);
			PopulateResourceFiles();
		}

		private void PopulateGaiaFiles()
		{
			lstGaiaImportant.get_Items().Clear();
			((ObjectCollection)chkGaiaExtras.get_Items()).Clear();
			GaiaLength = 0L;
			Gaia.GetAvailableGaiaCatalogues();
			for (int i = 0; i < Gaia.GaiaPrimaryFiles.Count; i++)
			{
				lstGaiaImportant.get_Items().Add((object)Gaia.GaiaPrimaryFiles[i]);
			}
			ExtraGaia = new List<FileNameDate>();
			string[] files = Directory.GetFiles(GaiaSourceDirectory, "*.bin");
			for (int j = 0; j < files.Length; j++)
			{
				EGaia = new FileNameDate();
				EGaia.FName = Path.GetFileName(files[j]);
				EGaia.Date = new DirectoryInfo(files[j]).LastWriteTime;
				EGaia.Size = new FileInfo(files[j]).Length;
				bool flag = true;
				for (int k = 0; k < lstGaiaImportant.get_Items().get_Count(); k++)
				{
					if (lstGaiaImportant.get_Items().get_Item(k).ToString()!.ToLower() + ".bin" == EGaia.FName.ToLower())
					{
						flag = false;
						break;
					}
				}
				if (flag)
				{
					ExtraGaia.Add(EGaia);
				}
			}
			ExtraGaia.Sort();
			((ObjectCollection)chkGaiaExtras.get_Items()).Clear();
			for (int l = 0; l < ExtraGaia.Count; l++)
			{
				((ObjectCollection)chkGaiaExtras.get_Items()).Add((object)ExtraGaia[l].FName);
				GaiaLength += ExtraGaia[l].Size + 519840;
			}
			GaiaSelectedLength();
		}

		private void PopulateKMZfiles()
		{
			string value = ((Control)txtNameContainsKMZ).get_Text().Trim().ToLower();
			((ObjectCollection)chkKMZ.get_Items()).Clear();
			KMZlength = 0L;
			FilterDateKMZ = new DateTime((int)updnYearKmz.get_Value(), (int)updnMonthKmz.get_Value(), 1);
			KMZfiles = new List<FileNameDate>();
			if (!Directory.Exists(KMZSourceDirectory))
			{
				return;
			}
			string[] files = Directory.GetFiles(KMZSourceDirectory, "*.*");
			for (int i = 0; i < files.Length; i++)
			{
				GEkmz = new FileNameDate();
				GEkmz.FName = Path.GetFileName(files[i]);
				GEkmz.Date = new DirectoryInfo(files[i]).LastWriteTime;
				GEkmz.Size = new FileInfo(files[i]).Length;
				if (files[i].ToLower().Contains(value) && GEkmz.Date < FilterDateKMZ)
				{
					KMZfiles.Add(GEkmz);
				}
			}
			KMZfiles.Sort();
			((ObjectCollection)chkKMZ.get_Items()).Clear();
			for (int j = 0; j < KMZfiles.Count; j++)
			{
				((ObjectCollection)chkKMZ.get_Items()).Add((object)KMZfiles[j].FName);
				KMZlength += KMZfiles[j].Size;
			}
			KMXSelectedLength();
		}

		private void PopulatePredictionFiles()
		{
			string value = ((Control)txtNameContainsPredict).get_Text().Trim().ToLower();
			((ObjectCollection)chkPredictions.get_Items()).Clear();
			PredictionLength = 0L;
			FilterDatePredict = new DateTime((int)updnYearPredict.get_Value(), (int)updnMonthPredict.get_Value(), 1);
			PredictionFiles = new List<FileNameDate>();
			string[] files = Directory.GetFiles(PredictionSourceDirectory, "*.*");
			for (int i = 0; i < files.Length; i++)
			{
				Predn = new FileNameDate();
				Predn.FName = Path.GetFileName(files[i]);
				Predn.Date = new DirectoryInfo(files[i]).LastWriteTime;
				Predn.Size = new FileInfo(files[i]).Length;
				if (files[i].ToLower().Contains(value) && Predn.Date < FilterDatePredict)
				{
					PredictionFiles.Add(Predn);
				}
			}
			PredictionFiles.Sort();
			((ObjectCollection)chkPredictions.get_Items()).Clear();
			for (int j = 0; j < PredictionFiles.Count; j++)
			{
				((ObjectCollection)chkPredictions.get_Items()).Add((object)PredictionFiles[j].FName);
				PredictionLength += PredictionFiles[j].Size;
			}
			PredictionSelectedLength();
		}

		private void PopulateObservationFiles()
		{
			string value = ((Control)txtNameContainsObs).get_Text().Trim().ToLower();
			((ObjectCollection)chkObservations.get_Items()).Clear();
			ObservationLength = 0L;
			FilterDateObserve = new DateTime((int)updnYearObservation.get_Value(), (int)updnMonthObservation.get_Value(), 1);
			ObservationFiles = new List<FileNameDate>();
			string[] files = Directory.GetFiles(ObservationSourceDirectory, "*.*");
			for (int i = 0; i < files.Length; i++)
			{
				Obsn = new FileNameDate();
				Obsn.FName = Path.GetFileName(files[i]);
				Obsn.Date = new DirectoryInfo(files[i]).LastWriteTime;
				Obsn.Size = new FileInfo(files[i]).Length;
				if (files[i].ToLower().Contains(value) && Obsn.Date < FilterDateObserve)
				{
					ObservationFiles.Add(Obsn);
				}
			}
			ObservationFiles.Sort();
			((ObjectCollection)chkObservations.get_Items()).Clear();
			for (int j = 0; j < ObservationFiles.Count; j++)
			{
				((ObjectCollection)chkObservations.get_Items()).Add((object)ObservationFiles[j].FName);
				ObservationLength += ObservationFiles[j].Size;
			}
			ObservationSelectedLength();
		}

		private void PopulateDownloadFiles()
		{
			string value = ((Control)txtNameContainsDownload).get_Text().Trim().ToLower();
			((ObjectCollection)chkDownload.get_Items()).Clear();
			DownloadLength = 0L;
			FilterDateDownload = new DateTime((int)updnYearDownload.get_Value(), (int)updnMonthDownload.get_Value(), 1);
			DownloadFiles = new List<FileNameDate>();
			string[] files = Directory.GetFiles(DownloadSourceDirectory);
			for (int i = 0; i < files.Length; i++)
			{
				Download = new FileNameDate();
				Download.FName = Path.GetFileName(files[i]);
				Download.Date = new DirectoryInfo(files[i]).LastWriteTime;
				Download.Size = new FileInfo(files[i]).Length;
				if (!files[i].ToLower().Contains(value))
				{
					continue;
				}
				bool flag = true;
				for (int j = 0; j < lstDownloadReserved.get_Items().get_Count(); j++)
				{
					if (lstDownloadReserved.get_Items().get_Item(j).ToString()!.ToLower() == Download.FName.ToLower())
					{
						flag = false;
						break;
					}
				}
				if (Download.Date < FilterDateDownload && flag)
				{
					DownloadFiles.Add(Download);
				}
			}
			DownloadFiles.Sort();
			((ObjectCollection)chkDownload.get_Items()).Clear();
			for (int k = 0; k < DownloadFiles.Count; k++)
			{
				((ObjectCollection)chkDownload.get_Items()).Add((object)DownloadFiles[k].FName);
				DownloadLength += DownloadFiles[k].Size;
			}
			DownloadSelectedLength();
		}

		private void PopulateResourceFiles()
		{
			string[] array = new string[153]
			{
				"2390000_XYZ.bin", "2400000_XYZ.bin", "2410000_XYZ.bin", "2420000_XYZ.bin", "2430000_XYZ.bin", "2440000_XYZ.bin", "2450000_XYZ.bin", "2460000_XYZ.bin", "2470000_XYZ.bin", "2480000_XYZ.bin",
				"addresses.txt", "Archive Graze Index.dat", "Archive Graze List.csv", "Archive ILOC Observers.dat", "Archive ILOC Sites.dat", "Archive Obs_MissingGrazes.dat", "Archive Observations 1600_1949.dat", "Archive Observations 1950_1969.dat", "Archive Observations 1970_1975.dat", "Archive Observations 1976_1980.dat",
				"Archive Observations 1981_1985.dat", "Archive Observations 1986_1989.dat", "Archive Observations 1990_1994.dat", "Archive Observations 1995_1999.dat", "Archive Observations 2000_2005.dat", "Archive Observations 2006_2015.dat", "Archive Observations 2016_2025.dat", "Archive Observations recent.dat", "Archive RGO Sites.dat", "Asteroid~Observations.xml",
				"AsteroidClasses.csv", "AsteroidDiameters.bin", "AsteroidDiametersAll.bin", "AsteroidDias_Indiv.bin", "AsteroidElements.csv", "AsteroidRegions.txt", "AsteroidRings.csv", "AsteroidsAlwaysRead.dat", "Bayer.bin", "BinaryAsteroid_SatelliteNames.txt",
				"BinaryAsteroids.csv", "CameraDelays.dat", "Constellation Abbrevs.dat", "Constellation id.dat", "Constellation Lines EW.dat", "Constellation Lines NS.dat", "Constellation Names.dat", "Crater.bin", "Crater.dat", "CraterTimings.bin",
				"DE_Ephemeris.bin", "DE_LongEphemeris.bin", "DE432.bin", "DE435.bin", "DE436.bin", "DE438.bin", "DeltaT.dat", "DeltaTA.dat", "downloadcontrol.txt", "Earth.bin",
				"Earth2014.SUR2014.1min.geod.bin", "EOP_2020plus.dat", "EOP_old.dat", "EOP_present.dat", "GeoidHeights.bin", "GoogleMap_FileFooter.txt", "GoogleMap_FileHeader.txt", "GoogleMap_ServerDriver_2.js", "GSC Fields.dat", "HGsHG1G2_Magnitudes.csv",
				"Hipparcos Index.dat", "ILOC Site Statistics.dat", "J6.bin", "J7.bin", "J8.bin", "J9.bin", "J10.bin", "J11.bin", "J12.bin", "J13.bin",
				"Jupiter.bin", "LOLA128.bin", "Lunar Reduction Email body.txt", "MARIA.DAT", "Mars.bin", "Mercury.bin", "MoonSeries.bin", "Neptune.bin", "Nutation.bin", "OldObservers.txt",
				"OldStars.txt", "Pluto.bin", "RArchive Observations 1600_1949.dat", "RArchive Observations 1950_1969.dat", "RArchive Observations 1970_1975.dat", "RArchive Observations 1976_1980.dat", "RArchive Observations 1981_1985.dat", "RArchive Observations 1986_1989.dat", "RArchive Observations 1990_1994.dat", "RArchive Observations 1995_1999.dat",
				"RArchive Observations 2000_2005.dat", "RArchive Observations 2006_2015.dat", "RArchive Observations 2016_2025.dat", "RArchive Observations recent.dat", "RGO Site Statistics.dat", "sao.inx", "sao_xz.dat", "sao1950.bin", "sat_uncertainties.csv", "Saturn.bin",
				"SaturnIX.bin", "StarDia.bin", "StarNames_Hipparcos.csv", "Tycho2.bin", "tycho2.inx", "U1.bin", "U2.bin", "U3.bin", "U4.bin", "U5.bin",
				"UCAC_BSS.bin", "UCAC_BSS.inx", "UCAC2 Index.dat", "Uranus.bin", "UserA.bin", "UserA.inx", "UserMinorPlanetElements.csv", "UserMinorPlanetElements_Lunar.csv", "UserStar_MinorPlanetSearch.dat", "Venus.bin",
				"WDS Discovery Codes.dat", "World.bin", "WORLD.INX", "xz_zc.dat", "xz80.dat", "xz80.inx", "XZ80index.dat", "XZ80Mag4.dat", "XZ80Mag4.inx", "XZ80Mag7.dat",
				"XZ80Mag7.inx", "XZ80Mag9.dat", "XZ80Mag9.inx", "XZConfirmations.DAT", "XZDoubles.dat", "XZDoubles Discoveries.dat", "XZinKepler2.dat", "XZNegatives.dat", "XZVariables.dat", "ZC.dat",
				"ZC.inx", "ZCNames.dat", "zc-xz.dat"
			};
			lstResourceReserved.get_Items().Clear();
			((ObjectCollection)chkResources_Extra.get_Items()).Clear();
			ResourceLength = 0L;
			string value = ((Control)txtNameContainsResource).get_Text().Trim().ToLower();
			FilterDateResource = new DateTime((int)updnYearResource.get_Value(), (int)updnMonthResource.get_Value(), 1);
			long num = 0L;
			for (int i = 0; i < array.Length; i++)
			{
				lstResourceReserved.get_Items().Add((object)array[i]);
				string text = ResourceSourceDirectory + "\\" + array[i];
				if (File.Exists(text))
				{
					num += new FileInfo(text).Length;
				}
			}
			((Control)lblReservedResourceFilesSize).set_Text(string.Format("Reserved file size:  {0,3:f1} MB", (double)num / 1048576.0));
			ExtraResourceFiles = new List<FileNameDate>();
			string[] files = Directory.GetFiles(ResourceSourceDirectory, "*.*");
			for (int j = 0; j < files.Length; j++)
			{
				EResource = new FileNameDate();
				EResource.FName = Path.GetFileName(files[j]);
				EResource.Date = new DirectoryInfo(files[j]).LastWriteTime;
				EResource.Size = new FileInfo(files[j]).Length;
				bool flag = true;
				for (int k = 0; k < array.Length; k++)
				{
					if (array[k].ToString().ToLower() == EResource.FName.ToLower())
					{
						flag = false;
						break;
					}
				}
				if (flag && files[j].ToLower().Contains(value) && EResource.Date < FilterDateResource)
				{
					ExtraResourceFiles.Add(EResource);
				}
			}
			ExtraResourceFiles.Sort();
			((ObjectCollection)chkResources_Extra.get_Items()).Clear();
			for (int l = 0; l < ExtraResourceFiles.Count; l++)
			{
				((ObjectCollection)chkResources_Extra.get_Items()).Add((object)ExtraResourceFiles[l].FName);
				ResourceLength += ExtraResourceFiles[l].Size;
			}
			ResourceSelectedLength();
		}

		private void cmdSetAllGaiaChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkGaiaExtras.get_Items()).get_Count(); i++)
			{
				chkGaiaExtras.SetItemChecked(i, true);
			}
			GaiaSelectedLength();
		}

		private void cmdClearGaiaChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkGaiaExtras.get_Items()).get_Count(); i++)
			{
				chkGaiaExtras.SetItemChecked(i, false);
			}
			GaiaSelectedLength();
		}

		private void chkLstGaiaExtras_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkGaiaExtras).get_SelectedIndex() >= 0 && chkMap.get_Checked())
			{
				try
				{
					((Form)Gaia.GaiaMap).Close();
				}
				catch
				{
				}
				GaiaCatalogueMap.Plot = true;
				Gaia.ShowGaiaMap();
				((Control)Gaia.GaiaMap).set_Left(((Control)this).get_Left() + ((Control)chkGaiaExtras).get_Right());
				((Control)Gaia.GaiaMap).set_Top(((Control)this).get_Top() + ((Control)chkGaiaExtras).get_Top());
				Gaia.GaiaMap.MapCatalogueCoverage(((ObjectCollection)chkGaiaExtras.get_Items()).get_Item(((ListControl)chkGaiaExtras).get_SelectedIndex()).ToString());
			}
		}

		private void lstGaiaImportant_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstGaiaImportant).get_SelectedIndex() >= 0)
			{
				GaiaCatalogueMap.Plot = true;
				Gaia.ShowGaiaMap();
				((Control)Gaia.GaiaMap).set_Left(((Control)this).get_Left() + ((Control)chkGaiaExtras).get_Right());
				((Control)Gaia.GaiaMap).set_Top(((Control)this).get_Top() + ((Control)chkGaiaExtras).get_Top());
				Gaia.GaiaMap.MapCatalogueCoverage(lstGaiaImportant.get_Items().get_Item(((ListControl)lstGaiaImportant).get_SelectedIndex()).ToString());
			}
		}

		private void updnYearKmz_ValueChanged(object sender, EventArgs e)
		{
			PopulateKMZfiles();
		}

		private void updnMonthKmz_ValueChanged(object sender, EventArgs e)
		{
			PopulateKMZfiles();
		}

		private void txtNameContainsKMZ_TextChanged(object sender, EventArgs e)
		{
			PopulateKMZfiles();
		}

		private void cmdClearKMZ_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkKMZ.get_Items()).get_Count(); i++)
			{
				chkKMZ.SetItemChecked(i, false);
			}
			KMXSelectedLength();
		}

		private void cmdSetAllKMZ_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkKMZ.get_Items()).get_Count(); i++)
			{
				chkKMZ.SetItemChecked(i, true);
			}
			KMXSelectedLength();
		}

		private void cmdSetAllPredictions_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkPredictions.get_Items()).get_Count(); i++)
			{
				chkPredictions.SetItemChecked(i, true);
			}
			PredictionSelectedLength();
		}

		private void cmdClearAllPredictions_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkPredictions.get_Items()).get_Count(); i++)
			{
				chkPredictions.SetItemChecked(i, false);
			}
			PredictionSelectedLength();
		}

		private void updnPredictYear_ValueChanged(object sender, EventArgs e)
		{
			PopulatePredictionFiles();
		}

		private void updnPredictMonth_ValueChanged(object sender, EventArgs e)
		{
			PopulatePredictionFiles();
		}

		private void txtNameContainsPredict_TextChanged(object sender, EventArgs e)
		{
			PopulatePredictionFiles();
		}

		private void cmdSetObservations_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkObservations.get_Items()).get_Count(); i++)
			{
				chkObservations.SetItemChecked(i, true);
			}
			ObservationSelectedLength();
		}

		private void cmdClearObservations_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkObservations.get_Items()).get_Count(); i++)
			{
				chkObservations.SetItemChecked(i, false);
			}
			ObservationSelectedLength();
		}

		private void updnYearObservation_ValueChanged(object sender, EventArgs e)
		{
			PopulateObservationFiles();
		}

		private void updnMonthObservation_ValueChanged(object sender, EventArgs e)
		{
			PopulateObservationFiles();
		}

		private void txtNameContainsObs_TextChanged(object sender, EventArgs e)
		{
			PopulateObservationFiles();
		}

		private void cmdSetDownload_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				chkDownload.SetItemChecked(i, true);
			}
			DownloadSelectedLength();
		}

		private void cmdClearDownload_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				chkDownload.SetItemChecked(i, false);
			}
			DownloadSelectedLength();
		}

		private void txtNameContainsDownload_TextChanged(object sender, EventArgs e)
		{
			PopulateDownloadFiles();
		}

		private void updnYearDownload_ValueChanged(object sender, EventArgs e)
		{
			PopulateDownloadFiles();
		}

		private void updnMonthDownload_ValueChanged(object sender, EventArgs e)
		{
			PopulateDownloadFiles();
		}

		private void cmdSetAllResourceChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkResources_Extra.get_Items()).get_Count(); i++)
			{
				chkResources_Extra.SetItemChecked(i, true);
			}
			ResourceSelectedLength();
		}

		private void updnYearResource_ValueChanged(object sender, EventArgs e)
		{
			PopulateResourceFiles();
		}

		private void updnMonthResource_ValueChanged(object sender, EventArgs e)
		{
			PopulateResourceFiles();
		}

		private void txtNameContainsResource_TextChanged(object sender, EventArgs e)
		{
			PopulateResourceFiles();
		}

		private void cmdClearAllResourceChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkResources_Extra.get_Items()).get_Count(); i++)
			{
				chkResources_Extra.SetItemChecked(i, false);
			}
			ResourceSelectedLength();
		}

		private void chkGaiaExtras_MouseUp(object sender, MouseEventArgs e)
		{
			GaiaSelectedLength();
		}

		private void GaiaSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < ExtraGaia.Count; i++)
			{
				if (chkGaiaExtras.GetItemChecked(i))
				{
					num += ExtraGaia[i].Size;
				}
			}
			((Control)lblGaiasize).set_Text(string.Format("File sizes:  Total {0,3:f1} MB   Selected {1,3:f1} MB", (double)GaiaLength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMoveGaia_Click(object sender, EventArgs e)
		{
			MoveDeleteGaia(Delete: false);
		}

		private void cmdDeleteGaia_Click(object sender, EventArgs e)
		{
			MoveDeleteGaia(Delete: true);
		}

		private void MoveDeleteGaia(bool Delete)
		{
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkGaiaExtras.get_Items()).get_Count(); i++)
			{
				if (chkGaiaExtras.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkGaiaExtras.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkGaiaExtras.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files.\r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm Gaia file deletions", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_Gaia))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_Gaia);
			}
			string text2 = "";
			string[] array = new string[3] { ".bin", ".inx", ".dat" };
			for (int j = 0; j < list.Count; j++)
			{
				for (int k = 0; k < 3; k++)
				{
					text2 = list[j].ToString() + array[k];
					try
					{
						if (k == 2)
						{
							text2 = "Hipparcos_" + text2;
						}
						if (Delete)
						{
							File.Delete(GaiaSourceDirectory + "\\" + text2);
						}
						else
						{
							File.Move(GaiaSourceDirectory + "\\" + text2, FilesForDeletionDirectory_Gaia + "\\" + text2);
						}
					}
					catch
					{
					}
				}
			}
			PopulateGaiaFiles();
		}

		private void chkKMZ_MouseUp(object sender, MouseEventArgs e)
		{
			KMXSelectedLength();
		}

		private void KMXSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < KMZfiles.Count; i++)
			{
				if (chkKMZ.GetItemChecked(i))
				{
					num += KMZfiles[i].Size;
				}
			}
			((Control)lblKMZsize).set_Text(string.Format("File sizes:  Total {0,3:f1} MB   Selected {1,3:f2} MB", (double)KMZlength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMoveKMZ_Click(object sender, EventArgs e)
		{
			MoveDeleteKMZ(Delete: false);
		}

		private void cmdDeleteKMZ_Click(object sender, EventArgs e)
		{
			MoveDeleteKMZ(Delete: true);
		}

		private void MoveDeleteKMZ(bool Delete)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkKMZ.get_Items()).get_Count(); i++)
			{
				if (chkKMZ.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkKMZ.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkKMZ.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files.\r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm KMZ file deletions", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_KMZ))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_KMZ);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						File.Delete(KMZSourceDirectory + "\\" + list[j]);
					}
					else
					{
						File.Move(KMZSourceDirectory + "\\" + list[j], FilesForDeletionDirectory_KMZ + "\\" + list[j]);
					}
				}
				catch
				{
				}
			}
			PopulateKMZfiles();
		}

		private void chkPredictions_MouseUp(object sender, MouseEventArgs e)
		{
			PredictionSelectedLength();
		}

		private void PredictionSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < PredictionFiles.Count; i++)
			{
				if (chkPredictions.GetItemChecked(i))
				{
					num += PredictionFiles[i].Size;
				}
			}
			((Control)lblPredictsize).set_Text(string.Format("File sizes:  Total {0,3:f1} MB   Selected {1,3:f1} MB", (double)PredictionLength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMovePredictions_Click(object sender, EventArgs e)
		{
			MoveDeletePredictions(Delete: false);
		}

		private void cmdDeletePredictions_Click(object sender, EventArgs e)
		{
			MoveDeletePredictions(Delete: true);
		}

		private void MoveDeletePredictions(bool Delete)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkPredictions.get_Items()).get_Count(); i++)
			{
				if (chkPredictions.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkPredictions.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkPredictions.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files. \r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_Predict))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_Predict);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						File.Delete(PredictionSourceDirectory + "\\" + list[j]);
					}
					File.Move(PredictionSourceDirectory + "\\" + list[j], FilesForDeletionDirectory_Predict + "\\" + list[j]);
				}
				catch
				{
				}
			}
			PopulatePredictionFiles();
		}

		private void chkObservations_MouseUp(object sender, MouseEventArgs e)
		{
			ObservationSelectedLength();
		}

		private void ObservationSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < ObservationFiles.Count; i++)
			{
				if (chkObservations.GetItemChecked(i))
				{
					num += ObservationFiles[i].Size;
				}
			}
			((Control)lblObservesize).set_Text(string.Format("File sizes:  Total {0,3:f2} MB   Selected {1,3:f2} MB", (double)ObservationLength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMoveObservations_Click(object sender, EventArgs e)
		{
			MoveDeleteObservations(Delete: false);
		}

		private void cmdDeleteObservations_Click(object sender, EventArgs e)
		{
			MoveDeleteObservations(Delete: true);
		}

		private void MoveDeleteObservations(bool Delete)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkObservations.get_Items()).get_Count(); i++)
			{
				if (chkObservations.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkObservations.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkObservations.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files. \r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_Observe))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_Observe);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						File.Delete(ObservationSourceDirectory + "\\" + list[j]);
					}
					File.Move(ObservationSourceDirectory + "\\" + list[j], FilesForDeletionDirectory_Observe + "\\" + list[j]);
				}
				catch
				{
				}
			}
			PopulateObservationFiles();
		}

		private void chkDownload_MouseUp(object sender, MouseEventArgs e)
		{
			DownloadSelectedLength();
		}

		private void DownloadSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < DownloadFiles.Count; i++)
			{
				if (chkDownload.GetItemChecked(i))
				{
					num += DownloadFiles[i].Size;
				}
			}
			((Control)lblDownloadsize).set_Text(string.Format("File sizes:  Total {0,3:f1} MB   Selected {1,3:f1} MB", (double)DownloadLength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMoveDownload_Click(object sender, EventArgs e)
		{
			MoveDeleteDownloads(Delete: false);
		}

		private void cmdDeleteDownload_Click(object sender, EventArgs e)
		{
			MoveDeleteDownloads(Delete: true);
		}

		private void MoveDeleteDownloads(bool Delete)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				if (chkDownload.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files. \r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_Download))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_Download);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						FileSystem.DeleteFile(DownloadSourceDirectory + "\\" + list[j], (UIOption)2, (RecycleOption)3);
					}
					else
					{
						File.Move(DownloadSourceDirectory + "\\" + list[j], FilesForDeletionDirectory_Download + "\\" + list[j]);
					}
				}
				catch
				{
				}
			}
			PopulateDownloadFiles();
		}

		private void chkResources_Extra_MouseUp(object sender, MouseEventArgs e)
		{
			ResourceSelectedLength();
		}

		private void ResourceSelectedLength()
		{
			long num = 0L;
			for (int i = 0; i < ExtraResourceFiles.Count; i++)
			{
				if (chkResources_Extra.GetItemChecked(i))
				{
					num += ExtraResourceFiles[i].Size;
				}
			}
			((Control)lblResourceSize).set_Text(string.Format("Non-reserved file sizes:  Total {0,3:f1} MB   Selected {1,3:f1} MB", (double)ResourceLength / 1048576.0, (double)num / 1048576.0));
		}

		private void cmdMoveResources_Click(object sender, EventArgs e)
		{
			MoveDeleteResource(Delete: false);
		}

		private void cmdDeleteResources_Click(object sender, EventArgs e)
		{
			MoveDeleteResource(Delete: true);
		}

		private void MoveDeleteResource(bool Delete)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkResources_Extra.get_Items()).get_Count(); i++)
			{
				if (chkResources_Extra.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkResources_Extra.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkResources_Extra.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the 'FilesForDeletion' folder:\r\n", list.Count) + text) : (string.Format("Do you want to permanently DELETE the following {0,1} files. \r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text));
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(FilesForDeletionDirectory_Resource))
			{
				Directory.CreateDirectory(FilesForDeletionDirectory_Resource);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						File.Delete(ResourceSourceDirectory + "\\" + list[j]);
					}
					File.Move(ResourceSourceDirectory + "\\" + list[j], FilesForDeletionDirectory_Resource + "\\" + list[j]);
				}
				catch
				{
				}
			}
			PopulateResourceFiles();
		}

		private void DisplayDownloads_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkDownload).get_SelectedIndex() >= 0)
			{
				DisplayFile(DownloadSourceDirectory + "\\" + ((ObjectCollection)chkDownload.get_Items()).get_Item(((ListControl)chkDownload).get_SelectedIndex()).ToString());
			}
		}

		private void cmdDisplayPredictions_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkPredictions).get_SelectedIndex() >= 0)
			{
				DisplayFile(PredictionSourceDirectory + "\\" + ((ObjectCollection)chkPredictions.get_Items()).get_Item(((ListControl)chkPredictions).get_SelectedIndex()).ToString());
			}
		}

		private void cmdDisplayObservations_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkObservations).get_SelectedIndex() >= 0)
			{
				DisplayFile(ObservationSourceDirectory + "\\" + ((ObjectCollection)chkObservations.get_Items()).get_Item(((ListControl)chkObservations).get_SelectedIndex()).ToString());
			}
		}

		private void cmdDisplayResources_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkResources_Extra).get_SelectedIndex() >= 0)
			{
				DisplayFile(ResourceSourceDirectory + "\\" + ((ObjectCollection)chkResources_Extra.get_Items()).get_Item(((ListControl)chkResources_Extra).get_SelectedIndex()).ToString());
			}
		}

		private void DisplayFile(string FilePath)
		{
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			string text = Path.GetExtension(FilePath)!.ToLower();
			if ((text == ".kmz") | (text == ".kml"))
			{
				GoogleEarth.DisplayGoogleMap(FilePath);
				return;
			}
			switch (text)
			{
			case ".bmp":
				Process.Start(FilePath);
				return;
			case ".png":
				Process.Start(FilePath);
				return;
			case ".jpg":
				Process.Start(FilePath);
				return;
			case ".gif":
				Process.Start(FilePath);
				return;
			}
			if ((text == ".zip") | (text == ".bin"))
			{
				MessageBox.Show("Cannot display files of type .bin or .zip", "Wrong file type", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			try
			{
				((Control)DisplayResources).Show();
			}
			catch
			{
				DisplayResources = new DisplayData();
				((Control)DisplayResources).Show();
			}
			int num = 50000;
			((Control)DisplayResources.txtBox).set_Text("");
			((Control)DisplayResources).set_Width(800);
			((Control)DisplayResources).set_Height(500);
			((Control)DisplayResources).set_Text("Text of " + FilePath);
			_ = new FileInfo(FilePath).Length;
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamReader streamReader = new StreamReader(FilePath))
			{
				string text2 = "Content of " + Path.GetFileName(FilePath) + "\r\n\r\n" + streamReader.ReadToEnd();
				if (text2.Length > num)
				{
					text2 = text2.Substring(0, num);
				}
				if (text2.Length == 50000)
				{
					text2 = "*** Display truncated to 50,000 characters ***\r\n\r\n" + text2;
				}
				((Control)DisplayResources.txtBox).set_Text(text2);
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdDisplayKMZ_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkKMZ).get_SelectedIndex() >= 0)
			{
				GoogleEarth.DisplayGoogleMap(KMZSourceDirectory + "\\" + ((ObjectCollection)chkKMZ.get_Items()).get_Item(((ListControl)chkKMZ).get_SelectedIndex()).ToString());
			}
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_0596: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a0: Expected O, but got Unknown
			//IL_0f17: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f21: Expected O, but got Unknown
			//IL_1bd9: Unknown result type (might be due to invalid IL or missing references)
			//IL_1be3: Expected O, but got Unknown
			//IL_28bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_28c7: Expected O, but got Unknown
			//IL_2bd6: Unknown result type (might be due to invalid IL or missing references)
			//IL_3703: Unknown result type (might be due to invalid IL or missing references)
			//IL_370d: Expected O, but got Unknown
			//IL_4108: Unknown result type (might be due to invalid IL or missing references)
			//IL_4112: Expected O, but got Unknown
			//IL_43a2: Unknown result type (might be due to invalid IL or missing references)
			chkGaiaExtras = new CheckedListBox();
			lstGaiaImportant = new ListBox();
			label1 = new Label();
			cmdSetAllGaiaChecks = new Button();
			cmdClearGaiaChecks = new Button();
			label2 = new Label();
			cmdMoveGaia = new Button();
			label3 = new Label();
			lblMoveNote = new Label();
			updnMonthKmz = new NumericUpDown();
			updnYearKmz = new NumericUpDown();
			label5 = new Label();
			cmdMoveKMZ = new Button();
			cmdClearKMZ = new Button();
			cmdSetAllKMZ = new Button();
			chkKMZ = new CheckedListBox();
			label4 = new Label();
			TabMoveDelete = new TabControl();
			tabPage5 = new TabPage();
			DisplayDownloads = new Button();
			lblDownloadsize = new Label();
			label21 = new Label();
			lstDownloadReserved = new ListBox();
			label17 = new Label();
			txtNameContainsDownload = new TextBox();
			label18 = new Label();
			cmdDeleteDownload = new Button();
			updnMonthDownload = new NumericUpDown();
			updnYearDownload = new NumericUpDown();
			label19 = new Label();
			label20 = new Label();
			cmdMoveDownload = new Button();
			chkDownload = new CheckedListBox();
			cmdClearDownload = new Button();
			cmdSetDownload = new Button();
			tabPage6 = new TabPage();
			cmdDisplayResources = new Button();
			label28 = new Label();
			lblReservedResourceFilesSize = new Label();
			label22 = new Label();
			txtNameContainsResource = new TextBox();
			label26 = new Label();
			updnMonthResource = new NumericUpDown();
			updnYearResource = new NumericUpDown();
			label27 = new Label();
			lblResourceSize = new Label();
			cmdDeleteResources = new Button();
			cmdMoveResources = new Button();
			label23 = new Label();
			label24 = new Label();
			label25 = new Label();
			chkResources_Extra = new CheckedListBox();
			cmdClearAllResourceChecks = new Button();
			lstResourceReserved = new ListBox();
			cmdSetAllResourceChecks = new Button();
			tabPage1 = new TabPage();
			chkMap = new CheckBox();
			lblGaiasize = new Label();
			cmdDeleteGaia = new Button();
			tabPage3 = new TabPage();
			cmdDisplayPredictions = new Button();
			lblPredictsize = new Label();
			label12 = new Label();
			txtNameContainsPredict = new TextBox();
			label13 = new Label();
			cmdDeletePredictions = new Button();
			updnMonthPredict = new NumericUpDown();
			updnYearPredict = new NumericUpDown();
			label6 = new Label();
			label7 = new Label();
			cmdMovePredictions = new Button();
			chkPredictions = new CheckedListBox();
			cmdClearAllPredictions = new Button();
			cmdSetAllPredictions = new Button();
			tabPage4 = new TabPage();
			cmdDisplayObservations = new Button();
			lblObservesize = new Label();
			label11 = new Label();
			txtNameContainsObs = new TextBox();
			label10 = new Label();
			cmdDeleteObservations = new Button();
			updnMonthObservation = new NumericUpDown();
			updnYearObservation = new NumericUpDown();
			label8 = new Label();
			label9 = new Label();
			cmdMoveObservations = new Button();
			chkObservations = new CheckedListBox();
			cmdClearObservations = new Button();
			cmdSetObservations = new Button();
			tabPage2 = new TabPage();
			cmdDisplayKMZ = new Button();
			lblKMZsize = new Label();
			label14 = new Label();
			txtNameContainsKMZ = new TextBox();
			label15 = new Label();
			cmdDeleteKMZ = new Button();
			label16 = new Label();
			((ISupportInitialize)updnMonthKmz).BeginInit();
			((ISupportInitialize)updnYearKmz).BeginInit();
			((Control)TabMoveDelete).SuspendLayout();
			((Control)tabPage5).SuspendLayout();
			((ISupportInitialize)updnMonthDownload).BeginInit();
			((ISupportInitialize)updnYearDownload).BeginInit();
			((Control)tabPage6).SuspendLayout();
			((ISupportInitialize)updnMonthResource).BeginInit();
			((ISupportInitialize)updnYearResource).BeginInit();
			((Control)tabPage1).SuspendLayout();
			((Control)tabPage3).SuspendLayout();
			((ISupportInitialize)updnMonthPredict).BeginInit();
			((ISupportInitialize)updnYearPredict).BeginInit();
			((Control)tabPage4).SuspendLayout();
			((ISupportInitialize)updnMonthObservation).BeginInit();
			((ISupportInitialize)updnYearObservation).BeginInit();
			((Control)tabPage2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)chkGaiaExtras).set_BackColor(Color.LavenderBlush);
			((Control)chkGaiaExtras).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkGaiaExtras).set_FormattingEnabled(true);
			((Control)chkGaiaExtras).set_Location(new Point(281, 96));
			((Control)chkGaiaExtras).set_Name("chkGaiaExtras");
			((Control)chkGaiaExtras).set_Size(new Size(154, 244));
			((Control)chkGaiaExtras).set_TabIndex(0);
			chkGaiaExtras.add_Click((EventHandler)chkLstGaiaExtras_Click);
			((Control)chkGaiaExtras).add_MouseUp(new MouseEventHandler(chkGaiaExtras_MouseUp));
			((Control)lstGaiaImportant).set_BackColor(Color.Honeydew);
			((Control)lstGaiaImportant).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstGaiaImportant).set_FormattingEnabled(true);
			((Control)lstGaiaImportant).set_Location(new Point(86, 96));
			((Control)lstGaiaImportant).set_Name("lstGaiaImportant");
			((Control)lstGaiaImportant).set_Size(new Size(154, 69));
			((Control)lstGaiaImportant).set_TabIndex(1);
			lstGaiaImportant.add_Click((EventHandler)lstGaiaImportant_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.MidnightBlue);
			((Control)label1).set_Location(new Point(302, 7));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(113, 26));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Gaia files");
			label1.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdSetAllGaiaChecks).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetAllGaiaChecks).set_Location(new Point(489, 96));
			((Control)cmdSetAllGaiaChecks).set_Name("cmdSetAllGaiaChecks");
			((Control)cmdSetAllGaiaChecks).set_Size(new Size(107, 26));
			((Control)cmdSetAllGaiaChecks).set_TabIndex(3);
			((Control)cmdSetAllGaiaChecks).set_Text("Set all checks");
			((ButtonBase)cmdSetAllGaiaChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAllGaiaChecks).add_Click((EventHandler)cmdSetAllGaiaChecks_Click);
			((Control)cmdClearGaiaChecks).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdClearGaiaChecks).set_Location(new Point(489, 142));
			((Control)cmdClearGaiaChecks).set_Name("cmdClearGaiaChecks");
			((Control)cmdClearGaiaChecks).set_Size(new Size(107, 26));
			((Control)cmdClearGaiaChecks).set_TabIndex(4);
			((Control)cmdClearGaiaChecks).set_Text("Clear all checks");
			((ButtonBase)cmdClearGaiaChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdClearGaiaChecks).add_Click((EventHandler)cmdClearGaiaChecks_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.DarkRed);
			((Control)label2).set_Location(new Point(288, 78));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(140, 17));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("Select for removal");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdMoveGaia).set_BackColor(Color.Gold);
			((Control)cmdMoveGaia).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMoveGaia).set_ForeColor(Color.Green);
			((Control)cmdMoveGaia).set_Location(new Point(481, 188));
			((Control)cmdMoveGaia).set_Name("cmdMoveGaia");
			((Control)cmdMoveGaia).set_Size(new Size(122, 65));
			((Control)cmdMoveGaia).set_TabIndex(7);
			((Control)cmdMoveGaia).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMoveGaia).set_UseVisualStyleBackColor(false);
			((Control)cmdMoveGaia).add_Click((EventHandler)cmdMoveGaia_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.MediumBlue);
			((Control)label3).set_Location(new Point(107, 77));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(112, 17));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("Reserved files");
			label3.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lblMoveNote).set_AutoSize(true);
			((Control)lblMoveNote).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblMoveNote).set_ForeColor(Color.DarkBlue);
			((Control)lblMoveNote).set_Location(new Point(25, 10));
			((Control)lblMoveNote).set_Name("lblMoveNote");
			((Control)lblMoveNote).set_Size(new Size(373, 17));
			((Control)lblMoveNote).set_TabIndex(7);
			((Control)lblMoveNote).set_Text("All files set for deletion are moved to the directory");
			lblMoveNote.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)updnMonthKmz).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)updnMonthKmz).set_Location(new Point(269, 65));
			updnMonthKmz.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonthKmz.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonthKmz).set_Name("updnMonthKmz");
			((Control)updnMonthKmz).set_Size(new Size(42, 23));
			((Control)updnMonthKmz).set_TabIndex(13);
			updnMonthKmz.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonthKmz.add_ValueChanged((EventHandler)updnMonthKmz_ValueChanged);
			((Control)updnYearKmz).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)updnYearKmz).set_Location(new Point(203, 65));
			updnYearKmz.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYearKmz.set_Minimum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnYearKmz).set_Name("updnYearKmz");
			((Control)updnYearKmz).set_Size(new Size(55, 23));
			((Control)updnYearKmz).set_TabIndex(12);
			updnYearKmz.set_Value(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnYearKmz.add_ValueChanged((EventHandler)updnYearKmz_ValueChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(50, 68));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(144, 17));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("List files dated before");
			((Control)cmdMoveKMZ).set_BackColor(Color.Gold);
			((Control)cmdMoveKMZ).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMoveKMZ).set_ForeColor(Color.Green);
			((Control)cmdMoveKMZ).set_Location(new Point(587, 174));
			((Control)cmdMoveKMZ).set_Name("cmdMoveKMZ");
			((Control)cmdMoveKMZ).set_Size(new Size(119, 69));
			((Control)cmdMoveKMZ).set_TabIndex(10);
			((Control)cmdMoveKMZ).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMoveKMZ).set_UseVisualStyleBackColor(false);
			((Control)cmdMoveKMZ).add_Click((EventHandler)cmdMoveKMZ_Click);
			((Control)cmdClearKMZ).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdClearKMZ).set_Location(new Point(593, 132));
			((Control)cmdClearKMZ).set_Name("cmdClearKMZ");
			((Control)cmdClearKMZ).set_Size(new Size(107, 26));
			((Control)cmdClearKMZ).set_TabIndex(9);
			((Control)cmdClearKMZ).set_Text("Clear all checks");
			((ButtonBase)cmdClearKMZ).set_UseVisualStyleBackColor(true);
			((Control)cmdClearKMZ).add_Click((EventHandler)cmdClearKMZ_Click);
			((Control)cmdSetAllKMZ).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetAllKMZ).set_Location(new Point(593, 90));
			((Control)cmdSetAllKMZ).set_Name("cmdSetAllKMZ");
			((Control)cmdSetAllKMZ).set_Size(new Size(107, 26));
			((Control)cmdSetAllKMZ).set_TabIndex(8);
			((Control)cmdSetAllKMZ).set_Text("Check all checks");
			((ButtonBase)cmdSetAllKMZ).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAllKMZ).add_Click((EventHandler)cmdSetAllKMZ_Click);
			((Control)chkKMZ).set_BackColor(Color.LavenderBlush);
			((Control)chkKMZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkKMZ).set_FormattingEnabled(true);
			((Control)chkKMZ).set_Location(new Point(50, 90));
			((Control)chkKMZ).set_Name("chkKMZ");
			((Control)chkKMZ).set_Size(new Size(513, 379));
			((Control)chkKMZ).set_TabIndex(4);
			((Control)chkKMZ).add_MouseUp(new MouseEventHandler(chkKMZ_MouseUp));
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.MidnightBlue);
			((Control)label4).set_Location(new Point(101, 7));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(411, 24));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text("GoogleEarth kmz files - for path predictions");
			label4.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage5);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage6);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage1);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage3);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage4);
			((Control)TabMoveDelete).get_Controls().Add((Control)(object)tabPage2);
			((Control)TabMoveDelete).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)TabMoveDelete).set_Location(new Point(1, 77));
			((Control)TabMoveDelete).set_Name("TabMoveDelete");
			TabMoveDelete.set_SelectedIndex(0);
			((Control)TabMoveDelete).set_Size(new Size(764, 521));
			((Control)TabMoveDelete).set_TabIndex(9);
			((Control)tabPage5).set_BackColor(Color.OldLace);
			((Panel)tabPage5).set_BorderStyle((BorderStyle)2);
			((Control)tabPage5).get_Controls().Add((Control)(object)DisplayDownloads);
			((Control)tabPage5).get_Controls().Add((Control)(object)lblDownloadsize);
			((Control)tabPage5).get_Controls().Add((Control)(object)label21);
			((Control)tabPage5).get_Controls().Add((Control)(object)lstDownloadReserved);
			((Control)tabPage5).get_Controls().Add((Control)(object)label17);
			((Control)tabPage5).get_Controls().Add((Control)(object)txtNameContainsDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)label18);
			((Control)tabPage5).get_Controls().Add((Control)(object)cmdDeleteDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)updnMonthDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)updnYearDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)label19);
			((Control)tabPage5).get_Controls().Add((Control)(object)label20);
			((Control)tabPage5).get_Controls().Add((Control)(object)cmdMoveDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)chkDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)cmdClearDownload);
			((Control)tabPage5).get_Controls().Add((Control)(object)cmdSetDownload);
			tabPage5.set_Location(new Point(4, 25));
			((Control)tabPage5).set_Name("tabPage5");
			((Control)tabPage5).set_Size(new Size(756, 492));
			tabPage5.set_TabIndex(4);
			((Control)tabPage5).set_Text("Downloads");
			((Control)DisplayDownloads).set_BackColor(Color.GreenYellow);
			((Control)DisplayDownloads).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)DisplayDownloads).set_Location(new Point(589, 421));
			((Control)DisplayDownloads).set_Name("DisplayDownloads");
			((Control)DisplayDownloads).set_Size(new Size(122, 38));
			((Control)DisplayDownloads).set_TabIndex(59);
			((Control)DisplayDownloads).set_Text("Display selected file");
			((ButtonBase)DisplayDownloads).set_UseVisualStyleBackColor(false);
			((Control)DisplayDownloads).add_Click((EventHandler)DisplayDownloads_Click);
			((Control)lblDownloadsize).set_AutoSize(true);
			((Control)lblDownloadsize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDownloadsize).set_ForeColor(Color.DarkGreen);
			((Control)lblDownloadsize).set_Location(new Point(50, 34));
			((Control)lblDownloadsize).set_Name("lblDownloadsize");
			((Control)lblDownloadsize).set_Size(new Size(176, 20));
			((Control)lblDownloadsize).set_TabIndex(49);
			((Control)lblDownloadsize).set_Text("Total file size = 0 MB");
			lblDownloadsize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_ForeColor(Color.DarkRed);
			((Control)label21).set_Location(new Point(53, 104));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(181, 13));
			((Control)label21).set_TabIndex(48);
			((Control)label21).set_Text("These important files will not be listed");
			label21.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lstDownloadReserved).set_BackColor(Color.LightCyan);
			((Control)lstDownloadReserved).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lstDownloadReserved).set_ForeColor(Color.DarkRed);
			((ListControl)lstDownloadReserved).set_FormattingEnabled(true);
			lstDownloadReserved.get_Items().AddRange(new object[20]
			{
				"AAVSOindex.dat", "Addresses.txt", "AstDyS2.dat", "astorb.dat", "Comet.dat", "CometLastObs.txt", "EOP_USNO_finals2000A.data", "eopc04_IAU2000.62-now", "int4_notes.txt", "int4_references.txt",
				"InterferometricCat.dat", "LC_Summary_Pub.txt", "MPCORB.DAT", "Reporting Addresses.txt", "SixthOrbitEclipticSubset.txt", "SixthOrbitCatalogue.txt", "updates.txt", "wds.dat", "WDSreferences.dat", "WeachartINIT.txt"
			});
			((Control)lstDownloadReserved).set_Location(new Point(60, 119));
			((Control)lstDownloadReserved).set_Name("lstDownloadReserved");
			((Control)lstDownloadReserved).set_Size(new Size(166, 277));
			((Control)lstDownloadReserved).set_TabIndex(47);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_ForeColor(Color.DarkRed);
			((Control)label17).set_Location(new Point(464, 37));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(115, 26));
			((Control)label17).set_TabIndex(46);
			((Control)label17).set_Text("No wild cards\r\nLeave blank to list ALL");
			label17.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtNameContainsDownload).set_Location(new Point(479, 65));
			((Control)txtNameContainsDownload).set_Name("txtNameContainsDownload");
			((Control)txtNameContainsDownload).set_Size(new Size(84, 23));
			((Control)txtNameContainsDownload).set_TabIndex(45);
			((Control)txtNameContainsDownload).add_TextChanged((EventHandler)txtNameContainsDownload_TextChanged);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(342, 68));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(128, 17));
			((Control)label18).set_TabIndex(44);
			((Control)label18).set_Text("List files containing");
			((Control)cmdDeleteDownload).set_BackColor(Color.Orange);
			((Control)cmdDeleteDownload).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteDownload).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteDownload).set_Location(new Point(585, 259));
			((Control)cmdDeleteDownload).set_Name("cmdDeleteDownload");
			((Control)cmdDeleteDownload).set_Size(new Size(122, 65));
			((Control)cmdDeleteDownload).set_TabIndex(43);
			((Control)cmdDeleteDownload).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeleteDownload).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteDownload).add_Click((EventHandler)cmdDeleteDownload_Click);
			((Control)updnMonthDownload).set_Location(new Point(269, 65));
			updnMonthDownload.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonthDownload.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonthDownload).set_Name("updnMonthDownload");
			((Control)updnMonthDownload).set_Size(new Size(42, 23));
			((Control)updnMonthDownload).set_TabIndex(42);
			updnMonthDownload.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonthDownload.add_ValueChanged((EventHandler)updnMonthDownload_ValueChanged);
			((Control)updnYearDownload).set_Location(new Point(203, 65));
			updnYearDownload.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYearDownload.set_Minimum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnYearDownload).set_Name("updnYearDownload");
			((Control)updnYearDownload).set_Size(new Size(55, 23));
			((Control)updnYearDownload).set_TabIndex(41);
			updnYearDownload.set_Value(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnYearDownload.add_ValueChanged((EventHandler)updnYearDownload_ValueChanged);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_ForeColor(Color.MidnightBlue);
			((Control)label19).set_Location(new Point(233, 7));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(146, 24));
			((Control)label19).set_TabIndex(35);
			((Control)label19).set_Text("Download files");
			label19.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(50, 68));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(144, 17));
			((Control)label20).set_TabIndex(40);
			((Control)label20).set_Text("List files dated before");
			((Control)cmdMoveDownload).set_BackColor(Color.Gold);
			((Control)cmdMoveDownload).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMoveDownload).set_ForeColor(Color.Green);
			((Control)cmdMoveDownload).set_Location(new Point(587, 174));
			((Control)cmdMoveDownload).set_Name("cmdMoveDownload");
			((Control)cmdMoveDownload).set_Size(new Size(119, 69));
			((Control)cmdMoveDownload).set_TabIndex(39);
			((Control)cmdMoveDownload).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMoveDownload).set_UseVisualStyleBackColor(false);
			((Control)cmdMoveDownload).add_Click((EventHandler)cmdMoveDownload_Click);
			((Control)chkDownload).set_BackColor(Color.LavenderBlush);
			((Control)chkDownload).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkDownload).set_FormattingEnabled(true);
			((Control)chkDownload).set_Location(new Point(304, 119));
			((Control)chkDownload).set_Name("chkDownload");
			((Control)chkDownload).set_Size(new Size(248, 364));
			((Control)chkDownload).set_TabIndex(36);
			((Control)chkDownload).add_MouseUp(new MouseEventHandler(chkDownload_MouseUp));
			((Control)cmdClearDownload).set_Location(new Point(593, 132));
			((Control)cmdClearDownload).set_Name("cmdClearDownload");
			((Control)cmdClearDownload).set_Size(new Size(107, 26));
			((Control)cmdClearDownload).set_TabIndex(38);
			((Control)cmdClearDownload).set_Text("Clear all checks");
			((ButtonBase)cmdClearDownload).set_UseVisualStyleBackColor(true);
			((Control)cmdClearDownload).add_Click((EventHandler)cmdClearDownload_Click);
			((Control)cmdSetDownload).set_Location(new Point(593, 90));
			((Control)cmdSetDownload).set_Name("cmdSetDownload");
			((Control)cmdSetDownload).set_Size(new Size(107, 26));
			((Control)cmdSetDownload).set_TabIndex(37);
			((Control)cmdSetDownload).set_Text("Check all checks");
			((ButtonBase)cmdSetDownload).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDownload).add_Click((EventHandler)cmdSetDownload_Click);
			((Control)tabPage6).set_BackColor(Color.OldLace);
			((Panel)tabPage6).set_BorderStyle((BorderStyle)2);
			((Control)tabPage6).get_Controls().Add((Control)(object)cmdDisplayResources);
			((Control)tabPage6).get_Controls().Add((Control)(object)label28);
			((Control)tabPage6).get_Controls().Add((Control)(object)lblReservedResourceFilesSize);
			((Control)tabPage6).get_Controls().Add((Control)(object)label22);
			((Control)tabPage6).get_Controls().Add((Control)(object)txtNameContainsResource);
			((Control)tabPage6).get_Controls().Add((Control)(object)label26);
			((Control)tabPage6).get_Controls().Add((Control)(object)updnMonthResource);
			((Control)tabPage6).get_Controls().Add((Control)(object)updnYearResource);
			((Control)tabPage6).get_Controls().Add((Control)(object)label27);
			((Control)tabPage6).get_Controls().Add((Control)(object)lblResourceSize);
			((Control)tabPage6).get_Controls().Add((Control)(object)cmdDeleteResources);
			((Control)tabPage6).get_Controls().Add((Control)(object)cmdMoveResources);
			((Control)tabPage6).get_Controls().Add((Control)(object)label23);
			((Control)tabPage6).get_Controls().Add((Control)(object)label24);
			((Control)tabPage6).get_Controls().Add((Control)(object)label25);
			((Control)tabPage6).get_Controls().Add((Control)(object)chkResources_Extra);
			((Control)tabPage6).get_Controls().Add((Control)(object)cmdClearAllResourceChecks);
			((Control)tabPage6).get_Controls().Add((Control)(object)lstResourceReserved);
			((Control)tabPage6).get_Controls().Add((Control)(object)cmdSetAllResourceChecks);
			tabPage6.set_Location(new Point(4, 25));
			((Control)tabPage6).set_Name("tabPage6");
			((Control)tabPage6).set_Size(new Size(756, 492));
			tabPage6.set_TabIndex(5);
			((Control)tabPage6).set_Text("Resource files");
			((Control)cmdDisplayResources).set_BackColor(Color.GreenYellow);
			((Control)cmdDisplayResources).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDisplayResources).set_Location(new Point(589, 421));
			((Control)cmdDisplayResources).set_Name("cmdDisplayResources");
			((Control)cmdDisplayResources).set_Size(new Size(122, 38));
			((Control)cmdDisplayResources).set_TabIndex(58);
			((Control)cmdDisplayResources).set_Text("Display selected file");
			((ButtonBase)cmdDisplayResources).set_UseVisualStyleBackColor(false);
			((Control)cmdDisplayResources).add_Click((EventHandler)cmdDisplayResources_Click);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_ForeColor(Color.DarkBlue);
			((Control)label28).set_Location(new Point(43, 103));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(233, 13));
			((Control)label28).set_TabIndex(57);
			((Control)label28).set_Text("Some files may not be on your computer");
			((Control)lblReservedResourceFilesSize).set_AutoSize(true);
			((Control)lblReservedResourceFilesSize).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblReservedResourceFilesSize).set_ForeColor(Color.DarkGreen);
			((Control)lblReservedResourceFilesSize).set_Location(new Point(50, 54));
			((Control)lblReservedResourceFilesSize).set_Name("lblReservedResourceFilesSize");
			((Control)lblReservedResourceFilesSize).set_Size(new Size(193, 17));
			((Control)lblReservedResourceFilesSize).set_TabIndex(56);
			((Control)lblReservedResourceFilesSize).set_Text("Reserved file size = 0 MB");
			lblReservedResourceFilesSize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_ForeColor(Color.DarkRed);
			((Control)label22).set_Location(new Point(593, 36));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(115, 26));
			((Control)label22).set_TabIndex(55);
			((Control)label22).set_Text("No wild cards\r\nLeave blank to list ALL");
			label22.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtNameContainsResource).set_Location(new Point(608, 83));
			((Control)txtNameContainsResource).set_Name("txtNameContainsResource");
			((Control)txtNameContainsResource).set_Size(new Size(84, 23));
			((Control)txtNameContainsResource).set_TabIndex(54);
			((Control)txtNameContainsResource).add_TextChanged((EventHandler)txtNameContainsResource_TextChanged);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(586, 65));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(128, 17));
			((Control)label26).set_TabIndex(53);
			((Control)label26).set_Text("List files containing");
			((Control)updnMonthResource).set_Location(new Point(510, 83));
			updnMonthResource.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonthResource.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonthResource).set_Name("updnMonthResource");
			((Control)updnMonthResource).set_Size(new Size(42, 23));
			((Control)updnMonthResource).set_TabIndex(52);
			updnMonthResource.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonthResource.add_ValueChanged((EventHandler)updnMonthResource_ValueChanged);
			((Control)updnYearResource).set_Location(new Point(444, 83));
			updnYearResource.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYearResource.set_Minimum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnYearResource).set_Name("updnYearResource");
			((Control)updnYearResource).set_Size(new Size(55, 23));
			((Control)updnYearResource).set_TabIndex(51);
			updnYearResource.set_Value(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnYearResource.add_ValueChanged((EventHandler)updnYearResource_ValueChanged);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(291, 86));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(144, 17));
			((Control)label27).set_TabIndex(50);
			((Control)label27).set_Text("List files dated before");
			((Control)lblResourceSize).set_AutoSize(true);
			((Control)lblResourceSize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblResourceSize).set_ForeColor(Color.DarkGreen);
			((Control)lblResourceSize).set_Location(new Point(50, 34));
			((Control)lblResourceSize).set_Name("lblResourceSize");
			((Control)lblResourceSize).set_Size(new Size(286, 20));
			((Control)lblResourceSize).set_TabIndex(49);
			((Control)lblResourceSize).set_Text("Total non-reserved file size = 0 MB");
			lblResourceSize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdDeleteResources).set_BackColor(Color.Orange);
			((Control)cmdDeleteResources).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteResources).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteResources).set_Location(new Point(589, 312));
			((Control)cmdDeleteResources).set_Name("cmdDeleteResources");
			((Control)cmdDeleteResources).set_Size(new Size(122, 65));
			((Control)cmdDeleteResources).set_TabIndex(48);
			((Control)cmdDeleteResources).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeleteResources).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteResources).add_Click((EventHandler)cmdDeleteResources_Click);
			((Control)cmdMoveResources).set_BackColor(Color.Gold);
			((Control)cmdMoveResources).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMoveResources).set_ForeColor(Color.Green);
			((Control)cmdMoveResources).set_Location(new Point(589, 227));
			((Control)cmdMoveResources).set_Name("cmdMoveResources");
			((Control)cmdMoveResources).set_Size(new Size(122, 65));
			((Control)cmdMoveResources).set_TabIndex(47);
			((Control)cmdMoveResources).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMoveResources).set_UseVisualStyleBackColor(false);
			((Control)cmdMoveResources).add_Click((EventHandler)cmdMoveResources_Click);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_ForeColor(Color.MediumBlue);
			((Control)label23).set_Location(new Point(103, 86));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(112, 17));
			((Control)label23).set_TabIndex(46);
			((Control)label23).set_Text("Reserved files");
			label23.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_ForeColor(Color.MidnightBlue);
			((Control)label24).set_Location(new Point(294, 7));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(164, 26));
			((Control)label24).set_TabIndex(42);
			((Control)label24).set_Text("Resource files");
			label24.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_ForeColor(Color.DarkRed);
			((Control)label25).set_Location(new Point(351, 63));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(140, 17));
			((Control)label25).set_TabIndex(45);
			((Control)label25).set_Text("Select for removal");
			label25.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)chkResources_Extra).set_BackColor(Color.LavenderBlush);
			((Control)chkResources_Extra).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkResources_Extra).set_FormattingEnabled(true);
			((Control)chkResources_Extra).set_Location(new Point(287, 109));
			((Control)chkResources_Extra).set_Name("chkResources_Extra");
			((Control)chkResources_Extra).set_Size(new Size(269, 364));
			((Control)chkResources_Extra).set_TabIndex(40);
			((Control)chkResources_Extra).add_MouseUp(new MouseEventHandler(chkResources_Extra_MouseUp));
			((Control)cmdClearAllResourceChecks).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdClearAllResourceChecks).set_Location(new Point(597, 181));
			((Control)cmdClearAllResourceChecks).set_Name("cmdClearAllResourceChecks");
			((Control)cmdClearAllResourceChecks).set_Size(new Size(107, 26));
			((Control)cmdClearAllResourceChecks).set_TabIndex(44);
			((Control)cmdClearAllResourceChecks).set_Text("Clear all");
			((ButtonBase)cmdClearAllResourceChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdClearAllResourceChecks).add_Click((EventHandler)cmdClearAllResourceChecks_Click);
			((Control)lstResourceReserved).set_BackColor(Color.Honeydew);
			((Control)lstResourceReserved).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResourceReserved).set_FormattingEnabled(true);
			((Control)lstResourceReserved).set_Location(new Point(50, 118));
			((Control)lstResourceReserved).set_Name("lstResourceReserved");
			((Control)lstResourceReserved).set_Size(new Size(219, 355));
			((Control)lstResourceReserved).set_TabIndex(41);
			((Control)cmdSetAllResourceChecks).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetAllResourceChecks).set_Location(new Point(597, 135));
			((Control)cmdSetAllResourceChecks).set_Name("cmdSetAllResourceChecks");
			((Control)cmdSetAllResourceChecks).set_Size(new Size(107, 26));
			((Control)cmdSetAllResourceChecks).set_TabIndex(43);
			((Control)cmdSetAllResourceChecks).set_Text("Check all");
			((ButtonBase)cmdSetAllResourceChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAllResourceChecks).add_Click((EventHandler)cmdSetAllResourceChecks_Click);
			((Control)tabPage1).set_BackColor(Color.OldLace);
			((Panel)tabPage1).set_BorderStyle((BorderStyle)2);
			((Control)tabPage1).get_Controls().Add((Control)(object)chkMap);
			((Control)tabPage1).get_Controls().Add((Control)(object)lblGaiasize);
			((Control)tabPage1).get_Controls().Add((Control)(object)cmdDeleteGaia);
			((Control)tabPage1).get_Controls().Add((Control)(object)cmdMoveGaia);
			((Control)tabPage1).get_Controls().Add((Control)(object)label3);
			((Control)tabPage1).get_Controls().Add((Control)(object)label1);
			((Control)tabPage1).get_Controls().Add((Control)(object)label2);
			((Control)tabPage1).get_Controls().Add((Control)(object)chkGaiaExtras);
			((Control)tabPage1).get_Controls().Add((Control)(object)cmdClearGaiaChecks);
			((Control)tabPage1).get_Controls().Add((Control)(object)lstGaiaImportant);
			((Control)tabPage1).get_Controls().Add((Control)(object)cmdSetAllGaiaChecks);
			((Control)tabPage1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			tabPage1.set_Location(new Point(4, 25));
			((Control)tabPage1).set_Name("tabPage1");
			((Control)tabPage1).set_Padding(new Padding(3));
			((Control)tabPage1).set_Size(new Size(756, 492));
			tabPage1.set_TabIndex(0);
			((Control)tabPage1).set_Text("Gaia files");
			((Control)chkMap).set_AutoSize(true);
			((Control)chkMap).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkMap).set_Location(new Point(281, 346));
			((Control)chkMap).set_Name("chkMap");
			((Control)chkMap).set_Size(new Size(188, 17));
			((Control)chkMap).set_TabIndex(40);
			((Control)chkMap).set_Text("Show star map when clicked");
			((ButtonBase)chkMap).set_UseVisualStyleBackColor(true);
			((Control)lblGaiasize).set_AutoSize(true);
			((Control)lblGaiasize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblGaiasize).set_ForeColor(Color.DarkGreen);
			((Control)lblGaiasize).set_Location(new Point(50, 34));
			((Control)lblGaiasize).set_Name("lblGaiasize");
			((Control)lblGaiasize).set_Size(new Size(176, 20));
			((Control)lblGaiasize).set_TabIndex(39);
			((Control)lblGaiasize).set_Text("Total file size = 0 MB");
			lblGaiasize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdDeleteGaia).set_BackColor(Color.Orange);
			((Control)cmdDeleteGaia).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteGaia).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteGaia).set_Location(new Point(481, 273));
			((Control)cmdDeleteGaia).set_Name("cmdDeleteGaia");
			((Control)cmdDeleteGaia).set_Size(new Size(122, 65));
			((Control)cmdDeleteGaia).set_TabIndex(8);
			((Control)cmdDeleteGaia).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeleteGaia).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteGaia).add_Click((EventHandler)cmdDeleteGaia_Click);
			((Control)tabPage3).set_BackColor(Color.OldLace);
			((Panel)tabPage3).set_BorderStyle((BorderStyle)2);
			((Control)tabPage3).get_Controls().Add((Control)(object)cmdDisplayPredictions);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblPredictsize);
			((Control)tabPage3).get_Controls().Add((Control)(object)label12);
			((Control)tabPage3).get_Controls().Add((Control)(object)txtNameContainsPredict);
			((Control)tabPage3).get_Controls().Add((Control)(object)label13);
			((Control)tabPage3).get_Controls().Add((Control)(object)cmdDeletePredictions);
			((Control)tabPage3).get_Controls().Add((Control)(object)updnMonthPredict);
			((Control)tabPage3).get_Controls().Add((Control)(object)updnYearPredict);
			((Control)tabPage3).get_Controls().Add((Control)(object)label6);
			((Control)tabPage3).get_Controls().Add((Control)(object)label7);
			((Control)tabPage3).get_Controls().Add((Control)(object)cmdMovePredictions);
			((Control)tabPage3).get_Controls().Add((Control)(object)chkPredictions);
			((Control)tabPage3).get_Controls().Add((Control)(object)cmdClearAllPredictions);
			((Control)tabPage3).get_Controls().Add((Control)(object)cmdSetAllPredictions);
			tabPage3.set_Location(new Point(4, 25));
			((Control)tabPage3).set_Name("tabPage3");
			((Control)tabPage3).set_Size(new Size(756, 492));
			tabPage3.set_TabIndex(2);
			((Control)tabPage3).set_Text("Predictions");
			((Control)cmdDisplayPredictions).set_BackColor(Color.GreenYellow);
			((Control)cmdDisplayPredictions).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDisplayPredictions).set_Location(new Point(589, 421));
			((Control)cmdDisplayPredictions).set_Name("cmdDisplayPredictions");
			((Control)cmdDisplayPredictions).set_Size(new Size(122, 38));
			((Control)cmdDisplayPredictions).set_TabIndex(59);
			((Control)cmdDisplayPredictions).set_Text("Display selected file");
			((ButtonBase)cmdDisplayPredictions).set_UseVisualStyleBackColor(false);
			((Control)cmdDisplayPredictions).add_Click((EventHandler)cmdDisplayPredictions_Click);
			((Control)lblPredictsize).set_AutoSize(true);
			((Control)lblPredictsize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblPredictsize).set_ForeColor(Color.DarkGreen);
			((Control)lblPredictsize).set_Location(new Point(50, 34));
			((Control)lblPredictsize).set_Name("lblPredictsize");
			((Control)lblPredictsize).set_Size(new Size(176, 20));
			((Control)lblPredictsize).set_TabIndex(40);
			((Control)lblPredictsize).set_Text("Total file size = 0 MB");
			lblPredictsize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_ForeColor(Color.DarkRed);
			((Control)label12).set_Location(new Point(464, 37));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(115, 26));
			((Control)label12).set_TabIndex(37);
			((Control)label12).set_Text("No wild cards\r\nLeave blank to list ALL");
			label12.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtNameContainsPredict).set_Location(new Point(479, 65));
			((Control)txtNameContainsPredict).set_Name("txtNameContainsPredict");
			((Control)txtNameContainsPredict).set_Size(new Size(84, 23));
			((Control)txtNameContainsPredict).set_TabIndex(36);
			((Control)txtNameContainsPredict).add_TextChanged((EventHandler)txtNameContainsPredict_TextChanged);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(342, 68));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(128, 17));
			((Control)label13).set_TabIndex(35);
			((Control)label13).set_Text("List files containing");
			((Control)cmdDeletePredictions).set_BackColor(Color.Orange);
			((Control)cmdDeletePredictions).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeletePredictions).set_ForeColor(Color.DarkRed);
			((Control)cmdDeletePredictions).set_Location(new Point(585, 259));
			((Control)cmdDeletePredictions).set_Name("cmdDeletePredictions");
			((Control)cmdDeletePredictions).set_Size(new Size(122, 65));
			((Control)cmdDeletePredictions).set_TabIndex(22);
			((Control)cmdDeletePredictions).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeletePredictions).set_UseVisualStyleBackColor(false);
			((Control)cmdDeletePredictions).add_Click((EventHandler)cmdDeletePredictions_Click);
			((Control)updnMonthPredict).set_Location(new Point(269, 65));
			updnMonthPredict.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonthPredict.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonthPredict).set_Name("updnMonthPredict");
			((Control)updnMonthPredict).set_Size(new Size(42, 23));
			((Control)updnMonthPredict).set_TabIndex(21);
			updnMonthPredict.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonthPredict.add_ValueChanged((EventHandler)updnPredictMonth_ValueChanged);
			((Control)updnYearPredict).set_Location(new Point(203, 65));
			updnYearPredict.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYearPredict.set_Minimum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnYearPredict).set_Name("updnYearPredict");
			((Control)updnYearPredict).set_Size(new Size(55, 23));
			((Control)updnYearPredict).set_TabIndex(20);
			updnYearPredict.set_Value(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnYearPredict.add_ValueChanged((EventHandler)updnPredictYear_ValueChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_ForeColor(Color.MidnightBlue);
			((Control)label6).set_Location(new Point(233, 7));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(147, 24));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("Prediction files");
			label6.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(50, 68));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(144, 17));
			((Control)label7).set_TabIndex(19);
			((Control)label7).set_Text("List files dated before");
			((Control)cmdMovePredictions).set_BackColor(Color.Gold);
			((Control)cmdMovePredictions).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMovePredictions).set_ForeColor(Color.Green);
			((Control)cmdMovePredictions).set_Location(new Point(587, 174));
			((Control)cmdMovePredictions).set_Name("cmdMovePredictions");
			((Control)cmdMovePredictions).set_Size(new Size(119, 69));
			((Control)cmdMovePredictions).set_TabIndex(18);
			((Control)cmdMovePredictions).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMovePredictions).set_UseVisualStyleBackColor(false);
			((Control)cmdMovePredictions).add_Click((EventHandler)cmdMovePredictions_Click);
			((Control)chkPredictions).set_BackColor(Color.LavenderBlush);
			((Control)chkPredictions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkPredictions).set_FormattingEnabled(true);
			((Control)chkPredictions).set_Location(new Point(50, 90));
			((Control)chkPredictions).set_Name("chkPredictions");
			((Control)chkPredictions).set_Size(new Size(513, 379));
			((Control)chkPredictions).set_TabIndex(15);
			((Control)chkPredictions).add_MouseUp(new MouseEventHandler(chkPredictions_MouseUp));
			((Control)cmdClearAllPredictions).set_Location(new Point(593, 132));
			((Control)cmdClearAllPredictions).set_Name("cmdClearAllPredictions");
			((Control)cmdClearAllPredictions).set_Size(new Size(107, 26));
			((Control)cmdClearAllPredictions).set_TabIndex(17);
			((Control)cmdClearAllPredictions).set_Text("Clear all checks");
			((ButtonBase)cmdClearAllPredictions).set_UseVisualStyleBackColor(true);
			((Control)cmdClearAllPredictions).add_Click((EventHandler)cmdClearAllPredictions_Click);
			((Control)cmdSetAllPredictions).set_Location(new Point(593, 90));
			((Control)cmdSetAllPredictions).set_Name("cmdSetAllPredictions");
			((Control)cmdSetAllPredictions).set_Size(new Size(107, 26));
			((Control)cmdSetAllPredictions).set_TabIndex(16);
			((Control)cmdSetAllPredictions).set_Text("Check all checks");
			((ButtonBase)cmdSetAllPredictions).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAllPredictions).add_Click((EventHandler)cmdSetAllPredictions_Click);
			((Control)tabPage4).set_BackColor(Color.OldLace);
			((Panel)tabPage4).set_BorderStyle((BorderStyle)2);
			((Control)tabPage4).get_Controls().Add((Control)(object)cmdDisplayObservations);
			((Control)tabPage4).get_Controls().Add((Control)(object)lblObservesize);
			((Control)tabPage4).get_Controls().Add((Control)(object)label11);
			((Control)tabPage4).get_Controls().Add((Control)(object)txtNameContainsObs);
			((Control)tabPage4).get_Controls().Add((Control)(object)label10);
			((Control)tabPage4).get_Controls().Add((Control)(object)cmdDeleteObservations);
			((Control)tabPage4).get_Controls().Add((Control)(object)updnMonthObservation);
			((Control)tabPage4).get_Controls().Add((Control)(object)updnYearObservation);
			((Control)tabPage4).get_Controls().Add((Control)(object)label8);
			((Control)tabPage4).get_Controls().Add((Control)(object)label9);
			((Control)tabPage4).get_Controls().Add((Control)(object)cmdMoveObservations);
			((Control)tabPage4).get_Controls().Add((Control)(object)chkObservations);
			((Control)tabPage4).get_Controls().Add((Control)(object)cmdClearObservations);
			((Control)tabPage4).get_Controls().Add((Control)(object)cmdSetObservations);
			tabPage4.set_Location(new Point(4, 25));
			((Control)tabPage4).set_Name("tabPage4");
			((Control)tabPage4).set_Size(new Size(756, 492));
			tabPage4.set_TabIndex(3);
			((Control)tabPage4).set_Text("Observations");
			((Control)cmdDisplayObservations).set_BackColor(Color.GreenYellow);
			((Control)cmdDisplayObservations).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDisplayObservations).set_Location(new Point(589, 421));
			((Control)cmdDisplayObservations).set_Name("cmdDisplayObservations");
			((Control)cmdDisplayObservations).set_Size(new Size(122, 38));
			((Control)cmdDisplayObservations).set_TabIndex(59);
			((Control)cmdDisplayObservations).set_Text("Display selected file");
			((ButtonBase)cmdDisplayObservations).set_UseVisualStyleBackColor(false);
			((Control)cmdDisplayObservations).add_Click((EventHandler)cmdDisplayObservations_Click);
			((Control)lblObservesize).set_AutoSize(true);
			((Control)lblObservesize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblObservesize).set_ForeColor(Color.DarkGreen);
			((Control)lblObservesize).set_Location(new Point(50, 34));
			((Control)lblObservesize).set_Name("lblObservesize");
			((Control)lblObservesize).set_Size(new Size(176, 20));
			((Control)lblObservesize).set_TabIndex(40);
			((Control)lblObservesize).set_Text("Total file size = 0 MB");
			lblObservesize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.DarkRed);
			((Control)label11).set_Location(new Point(464, 37));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(115, 26));
			((Control)label11).set_TabIndex(34);
			((Control)label11).set_Text("No wild cards\r\nLeave blank to list ALL");
			label11.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtNameContainsObs).set_Location(new Point(479, 65));
			((Control)txtNameContainsObs).set_Name("txtNameContainsObs");
			((Control)txtNameContainsObs).set_Size(new Size(84, 23));
			((Control)txtNameContainsObs).set_TabIndex(33);
			((Control)txtNameContainsObs).add_TextChanged((EventHandler)txtNameContainsObs_TextChanged);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(342, 68));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(128, 17));
			((Control)label10).set_TabIndex(32);
			((Control)label10).set_Text("List files containing");
			((Control)cmdDeleteObservations).set_BackColor(Color.Orange);
			((Control)cmdDeleteObservations).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteObservations).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteObservations).set_Location(new Point(585, 259));
			((Control)cmdDeleteObservations).set_Name("cmdDeleteObservations");
			((Control)cmdDeleteObservations).set_Size(new Size(122, 65));
			((Control)cmdDeleteObservations).set_TabIndex(31);
			((Control)cmdDeleteObservations).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeleteObservations).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteObservations).add_Click((EventHandler)cmdDeleteObservations_Click);
			((Control)updnMonthObservation).set_Location(new Point(269, 65));
			updnMonthObservation.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonthObservation.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonthObservation).set_Name("updnMonthObservation");
			((Control)updnMonthObservation).set_Size(new Size(42, 23));
			((Control)updnMonthObservation).set_TabIndex(30);
			updnMonthObservation.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonthObservation.add_ValueChanged((EventHandler)updnMonthObservation_ValueChanged);
			((Control)updnYearObservation).set_Location(new Point(203, 65));
			updnYearObservation.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYearObservation.set_Minimum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnYearObservation).set_Name("updnYearObservation");
			((Control)updnYearObservation).set_Size(new Size(55, 23));
			((Control)updnYearObservation).set_TabIndex(29);
			updnYearObservation.set_Value(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnYearObservation.add_ValueChanged((EventHandler)updnYearObservation_ValueChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_ForeColor(Color.MidnightBlue);
			((Control)label8).set_Location(new Point(224, 7));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(165, 24));
			((Control)label8).set_TabIndex(23);
			((Control)label8).set_Text("Observation files");
			label8.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(50, 68));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(144, 17));
			((Control)label9).set_TabIndex(28);
			((Control)label9).set_Text("List files dated before");
			((Control)cmdMoveObservations).set_BackColor(Color.Gold);
			((Control)cmdMoveObservations).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMoveObservations).set_ForeColor(Color.Green);
			((Control)cmdMoveObservations).set_Location(new Point(587, 174));
			((Control)cmdMoveObservations).set_Name("cmdMoveObservations");
			((Control)cmdMoveObservations).set_Size(new Size(119, 69));
			((Control)cmdMoveObservations).set_TabIndex(27);
			((Control)cmdMoveObservations).set_Text("Move to\r\n'For Deletion'\r\nfolder");
			((ButtonBase)cmdMoveObservations).set_UseVisualStyleBackColor(false);
			((Control)cmdMoveObservations).add_Click((EventHandler)cmdMoveObservations_Click);
			((Control)chkObservations).set_BackColor(Color.LavenderBlush);
			((Control)chkObservations).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkObservations).set_FormattingEnabled(true);
			((Control)chkObservations).set_Location(new Point(50, 90));
			((Control)chkObservations).set_Name("chkObservations");
			((Control)chkObservations).set_Size(new Size(513, 379));
			((Control)chkObservations).set_TabIndex(24);
			((Control)chkObservations).add_MouseUp(new MouseEventHandler(chkObservations_MouseUp));
			((Control)cmdClearObservations).set_Location(new Point(593, 132));
			((Control)cmdClearObservations).set_Name("cmdClearObservations");
			((Control)cmdClearObservations).set_Size(new Size(107, 26));
			((Control)cmdClearObservations).set_TabIndex(26);
			((Control)cmdClearObservations).set_Text("Clear all checks");
			((ButtonBase)cmdClearObservations).set_UseVisualStyleBackColor(true);
			((Control)cmdClearObservations).add_Click((EventHandler)cmdClearObservations_Click);
			((Control)cmdSetObservations).set_Location(new Point(593, 90));
			((Control)cmdSetObservations).set_Name("cmdSetObservations");
			((Control)cmdSetObservations).set_Size(new Size(107, 26));
			((Control)cmdSetObservations).set_TabIndex(25);
			((Control)cmdSetObservations).set_Text("Check all checks");
			((ButtonBase)cmdSetObservations).set_UseVisualStyleBackColor(true);
			((Control)cmdSetObservations).add_Click((EventHandler)cmdSetObservations_Click);
			((Control)tabPage2).set_BackColor(Color.OldLace);
			((Panel)tabPage2).set_BorderStyle((BorderStyle)2);
			((Control)tabPage2).get_Controls().Add((Control)(object)cmdDisplayKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)lblKMZsize);
			((Control)tabPage2).get_Controls().Add((Control)(object)label14);
			((Control)tabPage2).get_Controls().Add((Control)(object)txtNameContainsKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)label15);
			((Control)tabPage2).get_Controls().Add((Control)(object)cmdDeleteKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)updnMonthKmz);
			((Control)tabPage2).get_Controls().Add((Control)(object)updnYearKmz);
			((Control)tabPage2).get_Controls().Add((Control)(object)label4);
			((Control)tabPage2).get_Controls().Add((Control)(object)label5);
			((Control)tabPage2).get_Controls().Add((Control)(object)cmdMoveKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)chkKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)cmdClearKMZ);
			((Control)tabPage2).get_Controls().Add((Control)(object)cmdSetAllKMZ);
			((Control)tabPage2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			tabPage2.set_Location(new Point(4, 25));
			((Control)tabPage2).set_Name("tabPage2");
			((Control)tabPage2).set_Padding(new Padding(3));
			((Control)tabPage2).set_Size(new Size(756, 492));
			tabPage2.set_TabIndex(1);
			((Control)tabPage2).set_Text("Google Earth KMZ files");
			((Control)cmdDisplayKMZ).set_BackColor(Color.GreenYellow);
			((Control)cmdDisplayKMZ).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDisplayKMZ).set_Location(new Point(589, 421));
			((Control)cmdDisplayKMZ).set_Name("cmdDisplayKMZ");
			((Control)cmdDisplayKMZ).set_Size(new Size(122, 38));
			((Control)cmdDisplayKMZ).set_TabIndex(60);
			((Control)cmdDisplayKMZ).set_Text("Display selected file");
			((ButtonBase)cmdDisplayKMZ).set_UseVisualStyleBackColor(false);
			((Control)cmdDisplayKMZ).add_Click((EventHandler)cmdDisplayKMZ_Click);
			((Control)lblKMZsize).set_AutoSize(true);
			((Control)lblKMZsize).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblKMZsize).set_ForeColor(Color.DarkGreen);
			((Control)lblKMZsize).set_Location(new Point(50, 34));
			((Control)lblKMZsize).set_Name("lblKMZsize");
			((Control)lblKMZsize).set_Size(new Size(176, 20));
			((Control)lblKMZsize).set_TabIndex(38);
			((Control)lblKMZsize).set_Text("Total file size = 0 MB");
			lblKMZsize.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_ForeColor(Color.DarkRed);
			((Control)label14).set_Location(new Point(464, 37));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(115, 26));
			((Control)label14).set_TabIndex(37);
			((Control)label14).set_Text("No wild cards\r\nLeave blank to list ALL");
			label14.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtNameContainsKMZ).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtNameContainsKMZ).set_Location(new Point(479, 65));
			((Control)txtNameContainsKMZ).set_Name("txtNameContainsKMZ");
			((Control)txtNameContainsKMZ).set_Size(new Size(84, 23));
			((Control)txtNameContainsKMZ).set_TabIndex(36);
			((Control)txtNameContainsKMZ).add_TextChanged((EventHandler)txtNameContainsKMZ_TextChanged);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(342, 68));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(128, 17));
			((Control)label15).set_TabIndex(35);
			((Control)label15).set_Text("List files containing");
			((Control)cmdDeleteKMZ).set_BackColor(Color.Orange);
			((Control)cmdDeleteKMZ).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteKMZ).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteKMZ).set_Location(new Point(585, 259));
			((Control)cmdDeleteKMZ).set_Name("cmdDeleteKMZ");
			((Control)cmdDeleteKMZ).set_Size(new Size(122, 65));
			((Control)cmdDeleteKMZ).set_TabIndex(14);
			((Control)cmdDeleteKMZ).set_Text("Permanently\r\nDELETE\r\nselected files");
			((ButtonBase)cmdDeleteKMZ).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteKMZ).add_Click((EventHandler)cmdDeleteKMZ_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_ForeColor(Color.Red);
			((Control)label16).set_Location(new Point(97, 51));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(571, 17));
			((Control)label16).set_TabIndex(10);
			((Control)label16).set_Text("Files set for 'Permanently DELETE' cannot be recovered from the Recycle bin");
			label16.set_TextAlign(ContentAlignment.MiddleCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.FloralWhite);
			((Form)this).set_ClientSize(new Size(765, 600));
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)TabMoveDelete);
			((Control)this).get_Controls().Add((Control)(object)lblMoveNote);
			((Form)this).set_FormBorderStyle((FormBorderStyle)1);
			((Control)this).set_Name("DeleteSupercededFiles");
			((Control)this).set_Text("Move / Delete Superseded Files");
			((Form)this).add_Load((EventHandler)DeleteSupercededFiles_Load);
			((ISupportInitialize)updnMonthKmz).EndInit();
			((ISupportInitialize)updnYearKmz).EndInit();
			((Control)TabMoveDelete).ResumeLayout(false);
			((Control)tabPage5).ResumeLayout(false);
			((Control)tabPage5).PerformLayout();
			((ISupportInitialize)updnMonthDownload).EndInit();
			((ISupportInitialize)updnYearDownload).EndInit();
			((Control)tabPage6).ResumeLayout(false);
			((Control)tabPage6).PerformLayout();
			((ISupportInitialize)updnMonthResource).EndInit();
			((ISupportInitialize)updnYearResource).EndInit();
			((Control)tabPage1).ResumeLayout(false);
			((Control)tabPage1).PerformLayout();
			((Control)tabPage3).ResumeLayout(false);
			((Control)tabPage3).PerformLayout();
			((ISupportInitialize)updnMonthPredict).EndInit();
			((ISupportInitialize)updnYearPredict).EndInit();
			((Control)tabPage4).ResumeLayout(false);
			((Control)tabPage4).PerformLayout();
			((ISupportInitialize)updnMonthObservation).EndInit();
			((ISupportInitialize)updnYearObservation).EndInit();
			((Control)tabPage2).ResumeLayout(false);
			((Control)tabPage2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
