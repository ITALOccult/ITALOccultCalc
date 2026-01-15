using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace Occult.Lunar_Observations
{
	internal class OccultationReport
	{
		public ObserverLine Observer;

		public TelescopeLine Telescope;

		public EventLine Event;

		public List<ObserverLine> Observers = new List<ObserverLine>();

		public List<TelescopeLine> Telescopes = new List<TelescopeLine>();

		public List<EventLine> Events = new List<EventLine>();

		public static bool UseOldFormat;

		public string Place = "PLACE";

		public string Address = "ADDRESS";

		public string Representative = "REPRESENTATIVE";

		public string Reported = "REPORTED TO";

		public string EMail = "E-MAIL ADDRESS";

		public string Message = "";

		public void ReadReport(string ObservationFile)
		{
			string previousLineIn = "     ";
			using (StreamReader streamReader = new StreamReader(ObservationFile))
			{
				Observers.Clear();
				Telescopes.Clear();
				Events.Clear();
				do
				{
					string lineIn = streamReader.ReadLine()!.Replace('\t', ' ').PadRight(60);
					previousLineIn = DecodeLine(lineIn, previousLineIn, streamReader.EndOfStream);
				}
				while (!streamReader.EndOfStream);
			}
			FinaliseMessageRead();
			if (!UseOldFormat)
			{
				ReNumberEvents();
			}
		}

		public void ReadPastedReport()
		{
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			string previousLineIn = "     ";
			int num = -1;
			string text = Clipboard.GetText((TextDataFormat)0).Replace('\t', ' ');
			if (text.LastIndexOf('\r') < text.Length - 2)
			{
				text += "\r\n";
			}
			if (text.Length < 50)
			{
				MessageBox.Show("Text to be pasted is not valid.", "Invalid paste", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			Observers.Clear();
			Telescopes.Clear();
			Events.Clear();
			do
			{
				int num2 = ++num;
				num = text.IndexOf('\r', num2);
				if (text.Substring(num2, num - num2).IndexOf('\n') == 0)
				{
					num2++;
				}
				string lineIn = text.Substring(num2, num - num2).PadRight(60);
				previousLineIn = DecodeLine(lineIn, previousLineIn, num > text.Length - 3);
			}
			while (num < text.Length - 2);
			FinaliseMessageRead();
		}

		internal void FinaliseMessageRead()
		{
			int length = Message.Length;
			if (length >= 2 && Message.Substring(length - 2, 2) == "\r\n")
			{
				Message = Message.Substring(0, length - 2);
			}
		}

		internal string DecodeLine(string LineIn, string PreviousLineIn, bool AtEndOfData)
		{
			if (!(LineIn.Substring(0, 3) == "***"))
			{
				if (LineIn.Substring(0, 5).ToUpper() == "PLACE")
				{
					Place = LineIn.Substring(15).Trim();
					if (Place.ToUpper() == Place)
					{
						Place = Utilities.ProperCase(Place);
					}
				}
				else if ((LineIn.Substring(0, 5).ToUpper() == "ADDRE") | (LineIn.Substring(3, 4).ToUpper() == "RESS"))
				{
					Address = LineIn.Substring(15).Trim();
				}
				else if ((LineIn.Substring(0, 4).ToUpper() == "E-MA") | (LineIn.Substring(0, 5).ToUpper() == "EMAIL") | LineIn.Substring(0, 5).ToUpper().Contains("MAI"))
				{
					EMail = LineIn.Substring(15).Trim();
				}
				else if (LineIn.Substring(0, 5).ToUpper() == "REPRE")
				{
					Representative = LineIn.Substring(15).Trim();
					if (Representative.ToUpper() == Representative)
					{
						Representative = Utilities.ProperCase(Representative);
					}
				}
				else if (LineIn.Substring(0, 5).ToUpper() == "REPOR")
				{
					Reported = LineIn.Substring(15).Trim();
				}
				else if (LineIn.Substring(0, 5).ToUpper() == "MESSA")
				{
					string text = LineIn.Substring(15).Trim();
					if (Message.Length == 0)
					{
						Message = text;
					}
					else
					{
						Message += text;
					}
					if (text.Length < 48)
					{
						Message += "\r\n";
					}
					else
					{
						Message += " ";
					}
				}
				else if (LineIn.Substring(0, 1) == "T")
				{
					Telescope = new TelescopeLine();
					if (Telescope.DecodeTelescopeLine(LineIn))
					{
						Telescopes.Add(Telescope);
					}
				}
				else if (LineIn.Substring(0, 1) == "O")
				{
					Observer = new ObserverLine();
					Observer.DecodeObserverLine(LineIn);
					Observers.Add(Observer);
				}
				else if (!(LineIn.Substring(0, 1) == "M") && !(LineIn.Substring(0, 1) == "G") && LineIn.Length > 0)
				{
					if (LineIn.Substring(0, 2).Contains("@"))
					{
						LineIn = "      " + LineIn;
					}
					if (PreviousLineIn.Substring(0, 2).Trim().Length > 0)
					{
						Event = new EventLine();
						if (LineIn.Substring(0, 2).Trim().Length > 0)
						{
							Event.DecodeObservationLine_Occult(PreviousLineIn, "");
						}
						else
						{
							Event.DecodeObservationLine_Occult(PreviousLineIn, LineIn);
						}
						Events.Add(Event);
					}
					if (LineIn.Substring(0, 2).Trim().Length > 0 && AtEndOfData)
					{
						Event = new EventLine();
						Event.DecodeObservationLine_Occult(LineIn, "");
						Events.Add(Event);
					}
					PreviousLineIn = LineIn;
				}
			}
			return PreviousLineIn;
		}

		public void AddNewTelescopeLine(string LineIn)
		{
			Telescope = new TelescopeLine();
			if (Telescope.DecodeTelescopeLine(LineIn))
			{
				Telescopes.Add(Telescope);
			}
		}

		public void AddNewObserverLine(string LineIn)
		{
			Observer = new ObserverLine();
			Observer.DecodeObserverLine(LineIn);
			Observers.Add(Observer);
		}

		public void AddNewEventLine(string LineIn, string CommentLine)
		{
			Event = new EventLine();
			Event.DecodeObservationLine_Occult(LineIn, CommentLine);
			Events.Add(Event);
		}

		public void WriteReport(string ObservationFile, bool UseOldFormat)
		{
			EventLine.UseOldFormat = UseOldFormat;
			TelescopeLine.UseOldFormat = UseOldFormat;
			ObserverLine.UseOldFormat = UseOldFormat;
			using StreamWriter streamWriter = new StreamWriter(ObservationFile);
			streamWriter.WriteLine("Place name     " + Place.PadRight(50));
			if (UseOldFormat)
			{
				streamWriter.WriteLine("Address        " + Address);
			}
			streamWriter.WriteLine("Email address  " + EMail.PadRight(60));
			streamWriter.WriteLine("Representative " + Representative.PadRight(60));
			if (UseOldFormat)
			{
				streamWriter.WriteLine("Reported to    " + Reported);
			}
			streamWriter.WriteLine("");
			if (Message.Length > 0)
			{
				while (Message.IndexOf("\r\n\r\n") > 0)
				{
					Message = Message.Replace("\r\n\r\n", "\r\n");
				}
				int num = 0;
				int length = Message.Length;
				int num2 = 0;
				int num3 = 1;
				do
				{
					num2 = ((length - num >= 60) ? Message.LastIndexOf(" ", num + 60) : length);
					int num4 = Message.Substring(num, num2 - num).IndexOf("\r\n");
					num3 = 1;
					if (num4 > 0 && num4 < num2 - 2)
					{
						num2 = num + num4;
						num3 = 2;
					}
					streamWriter.WriteLine("Message        " + Message.Substring(num, num2 - num).PadRight(60));
					num = num2 + num3;
				}
				while (num2 < length);
				streamWriter.WriteLine("");
			}
			for (int i = 0; i < Telescopes.Count; i++)
			{
				streamWriter.WriteLine(Telescopes[i].ToString());
			}
			streamWriter.WriteLine("");
			for (int j = 0; j < Observers.Count; j++)
			{
				streamWriter.WriteLine(Observers[j].ToString());
			}
			streamWriter.WriteLine("");
			for (int k = 0; k < Events.Count; k++)
			{
				Events[k].GetEventLines(out var Line, out var Line2);
				streamWriter.WriteLine(Line);
				if (Line2.Trim().Length > 0)
				{
					streamWriter.WriteLine(Line2);
				}
			}
		}

		public string ArchiveLine(int Line)
		{
			string text = "";
			string text2 = "";
			string text3 = "";
			string text4 = "";
			StringBuilder stringBuilder = new StringBuilder();
			EventLine.UseOldFormat = (TelescopeLine.UseOldFormat = (ObserverLine.UseOldFormat = false));
			text = Events[Line].ToString();
			string text5 = text.Substring(59, 1);
			string text6 = text.Substring(60, 1);
			for (int i = 0; i < Telescopes.Count; i++)
			{
				if (text5 == Telescopes[i].TelescopeCodeForEvent)
				{
					text2 = Telescopes[i].ToString();
					text4 = Telescopes[i].TelescopePlace;
					break;
				}
			}
			for (int i = 0; i < Observers.Count; i++)
			{
				if (text6 == Observers[i].ObserverCodeForEvent)
				{
					text3 = Utilities.ProperCase(Observers[i].ObserverName.ToString()).Trim().PadRight(25)
						.Substring(0, 25);
					break;
				}
			}
			if (text2.Length > 0)
			{
				stringBuilder.Append(text.Substring(0, 59));
				stringBuilder.Append("A".PadLeft(13).PadRight(22));
				stringBuilder.Append(text2.Substring(20, 11));
				stringBuilder.Append(text2.Substring(32, 10));
				stringBuilder.Append(text2.Substring(43, 2));
				stringBuilder.Append(text2.Substring(46, 6));
				stringBuilder.Append(text2.Substring(52, 1));
				stringBuilder.Append("".PadRight(16));
				if (text4.Trim().Length > 0)
				{
					stringBuilder.Append(Utilities.ProperCase(text4).Trim().PadRight(50)
						.Substring(0, 50));
				}
				else
				{
					stringBuilder.Append(Utilities.ProperCase(Place).Trim().PadRight(50)
						.Substring(0, 50));
				}
				stringBuilder.Append(text2.Substring(4, 3));
				stringBuilder.Append(text2.Substring(8, 4));
				stringBuilder.Append(text2.Substring(14, 4));
				stringBuilder.Append(text3.PadRight(25).Substring(0, 25));
				stringBuilder.Append(text.Substring(0, 25));
				stringBuilder.Append(text2.Substring(20, 11));
				stringBuilder.Append(text2.Substring(32, 10));
				stringBuilder.Append(text2.Substring(43, 2));
				stringBuilder.Append(text2.Substring(46, 6));
				stringBuilder.Append(text2.Substring(52, 1));
				return stringBuilder.ToString();
			}
			return "";
		}

		public void AppendReportToGrazeFiles(string GrazeFile)
		{
			EventLine.UseOldFormat = true;
			TelescopeLine.UseOldFormat = true;
			ObserverLine.UseOldFormat = true;
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\" + GrazeFile, append: true);
			for (int i = 0; i < Telescopes.Count; i++)
			{
				streamWriter.WriteLine(Telescopes[i].ToString());
			}
			for (int j = 0; j < Observers.Count; j++)
			{
				streamWriter.WriteLine(Observers[j].ToString());
			}
			for (int k = 0; k < Events.Count; k++)
			{
				Events[k].GetEventLines(out var Line, out var Line2);
				streamWriter.WriteLine(Line);
				if (Line2.Trim().Length > 0)
				{
					streamWriter.WriteLine(Line2);
				}
			}
			streamWriter.WriteLine("***");
		}

		internal string GetNextSiteID()
		{
			_ = new char[0];
			int num = 0;
			int num2 = 64;
			for (int i = 0; i < Telescopes.Count; i++)
			{
				num = Telescopes[i].TelescopeCodeForEvent.ToCharArray()[0];
				if (num > num2)
				{
					num2 = num;
				}
			}
			num2++;
			if (num2 > 90 && num2 < 97)
			{
				num2 = 97;
			}
			if (num2 > 126 && num2 < 192)
			{
				num2 = 192;
			}
			return Convert.ToString((char)num2);
		}

		internal void MoveSiteUp(int RecordToMove)
		{
			if (RecordToMove >= 1)
			{
				TelescopeLine telescopeLine = new TelescopeLine();
				telescopeLine = Telescopes[RecordToMove];
				Telescopes[RecordToMove] = Telescopes[RecordToMove - 1];
				Telescopes[RecordToMove - 1] = telescopeLine;
			}
		}

		internal void MoveSiteDown(int RecordToMove)
		{
			if (!(RecordToMove > Observers.Count - 2 || RecordToMove < 0))
			{
				TelescopeLine telescopeLine = new TelescopeLine();
				telescopeLine = Telescopes[RecordToMove];
				Telescopes[RecordToMove] = Telescopes[RecordToMove + 1];
				Telescopes[RecordToMove + 1] = telescopeLine;
			}
		}

		internal int AddNewSite(string LineIn)
		{
			Telescope = new TelescopeLine();
			Telescope.DecodeTelescopeLine(LineIn.PadRight(80).Remove(1, 1).Insert(1, GetNextSiteID()));
			Telescopes.Add(Telescope);
			return Telescopes.Count - 1;
		}

		internal void ReNumberSites(string FirstCharacter)
		{
			string[] array = new string[60];
			string[] array2 = new string[60];
			_ = new char[0];
			int num = FirstCharacter.ToCharArray()[0];
			for (int i = 0; i < Telescopes.Count; i++)
			{
				array[i] = Telescopes[i].TelescopeCodeForEvent;
				array2[i] = Convert.ToString((char)num);
				Telescopes[i].TelescopeCodeForEvent = array2[i];
				num++;
				if (num > 90 && num < 97)
				{
					num = 97;
				}
				if (num > 126 && num < 192)
				{
					num = 192;
				}
			}
			for (int j = 0; j < Events.Count; j++)
			{
				string eventTelescope = Events[j].EventTelescope;
				for (int k = 0; k < Telescopes.Count; k++)
				{
					if (eventTelescope == array[k])
					{
						Events[j].EventTelescope = array2[k];
						break;
					}
				}
			}
			EventLine.SortField = 1;
			Events.Sort();
			for (int l = 0; l < Events.Count; l++)
			{
				Events[l].SeqNumber = l % 100 + 1;
			}
		}

		internal string GetNextNameID()
		{
			_ = new char[0];
			int num = 0;
			int num2 = 64;
			for (int i = 0; i < Observers.Count; i++)
			{
				num = Observers[i].ObserverCodeForEvent.ToCharArray()[0];
				if (num > num2)
				{
					num2 = num;
				}
			}
			num2++;
			if (num2 > 90 && num2 < 97)
			{
				num2 = 97;
			}
			if (num2 > 126 && num2 < 192)
			{
				num2 = 192;
			}
			return Convert.ToString((char)num2);
		}

		internal int AddNewName(string LineIn)
		{
			Observer = new ObserverLine();
			Observer.DecodeObserverLine(LineIn.PadRight(80).Remove(1, 1).Insert(1, GetNextNameID()));
			Observers.Add(Observer);
			return Observers.Count - 1;
		}

		internal void ReNumberNames(string FirstCharacter)
		{
			string[] array = new string[60];
			string[] array2 = new string[60];
			_ = new char[0];
			int num = FirstCharacter.ToCharArray()[0];
			for (int i = 0; i < Observers.Count; i++)
			{
				array[i] = Observers[i].ObserverCodeForEvent;
				array2[i] = Convert.ToString((char)num);
				Observers[i].ObserverCodeForEvent = array2[i];
				num++;
				if (num > 90 && num < 97)
				{
					num = 97;
				}
				if (num > 126 && num < 192)
				{
					num = 192;
				}
			}
			for (int j = 0; j < Events.Count; j++)
			{
				string eventObserver = Events[j].EventObserver;
				for (int k = 0; k < Observers.Count; k++)
				{
					if (eventObserver == array[k])
					{
						Events[j].EventObserver = array2[k];
						break;
					}
				}
			}
			for (int l = 0; l < Events.Count; l++)
			{
				string eventObserver = Events[l].EventRecorder;
				for (int m = 0; m < Observers.Count; m++)
				{
					if (eventObserver == array[m])
					{
						Events[l].EventRecorder = array2[m];
						break;
					}
				}
			}
		}

		internal void MoveNameUp(int RecordToMove)
		{
			if (RecordToMove >= 1)
			{
				ObserverLine observerLine = new ObserverLine();
				observerLine = Observers[RecordToMove];
				Observers[RecordToMove] = Observers[RecordToMove - 1];
				Observers[RecordToMove - 1] = observerLine;
			}
		}

		internal void MoveNameDown(int RecordToMove)
		{
			if (!(RecordToMove > Observers.Count - 2 || RecordToMove < 0))
			{
				ObserverLine observerLine = new ObserverLine();
				observerLine = Observers[RecordToMove];
				Observers[RecordToMove] = Observers[RecordToMove + 1];
				Observers[RecordToMove + 1] = observerLine;
			}
		}

		internal int AddNewEvent(string LineIn)
		{
			Event = new EventLine();
			Event.DecodeObservationLine_Occult("", "");
			Event.SeqNumber = (Events.Count + 1) % 100;
			Events.Add(Event);
			return Events.Count - 1;
		}

		internal void ReNumberEvents()
		{
			for (int i = 0; i < Events.Count; i++)
			{
				if (UseOldFormat)
				{
					Events[i].SeqNumber = i % 100 + 1;
				}
				else
				{
					Events[i].SeqNumber = i % 10000 + 1;
				}
			}
		}
	}
}
